# Subset the combined daily data set based on completeness criteria ----
 # {{ + initiate the temporal loop + }} ----
library(doParallel)

maxgap <- 3L

cl <- makeCluster(cores, outfile = file.path(runtime_path, paste0("foreach_logs_",
                                                                   time_period,
                                                                   timestamp,
                                                                   ".txt")))
print(cl)
 
 # register the cluster
registerDoParallel(cl)

## Loop through completeness thresholds ----

## 0:65 / 2:75 / 4:85 / 6:95
foreach(i = c(0, 2, 4, 6),
        .multicombine = TRUE,
        .verbose = TRUE,
        .packages =c("here", "data.table", "dplyr",
                     "imputeTS", "weathermetrics", "HeatStress",
                     "tictoc", "logger")
        ) %dopar% {


  library(here)
  library(dplyr)
  library(data.table)
  library(logger)
  library(imputeTS)
  library(HeatStress)

  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)
          
  source(here::here("runtime_setup",
                    "1_helper_functions_geo.R"), local = T)

  threshold_label <- (100-((maxcode - i) * 5)) 
  print(threshold_label)
          
  ## create a folder storing the log files
      logger_file <- file.path(meta_output_path, paste0("logger_outputs_",
                                                        threshold_label,
                                                    ".log"))
      ## define the file logger with log rotation enabled
      log_appender(appender_file(logger_file, max_files = 1L,
                                 max_lines = Inf, max_bytes = Inf))
      
      log_threshold(TRACE)
      log_threshold()
      
  gc()
  
  plots_output_path <- file.path(reports_path, paste0("Plots"))

   tryCatch({
  # * 1----

  qc_annual_summaries <- all_annual_summaries %>%
    dplyr::filter(completeness_code >= i)
  glimpse(qc_annual_summaries)

  qc_daily_summaries_int1_pass_completeness <- all_daily_summaries_inf2na %>%
    dplyr::filter(completeness_code >= i) %>% 
    arrange(station_id, YYYY_MM_DD) #%>% 
  
  length(unique(qc_daily_summaries_int1_pass_completeness$station_id))

  all_daily_inf2na_notimputed_count <- qc_daily_summaries_int1_pass_completeness %>%
    group_by(YYYY) %>%
    summarise(
      n_records = n(),
      n_distinct_stations = n_distinct(station_id),
      across(all_of(selected_variables),
             list(
               null = ~sum(is.na(.)),
               inf = ~sum(is.infinite(.)),
               min = ~round(min(., na.rm = T), 1),
               max = ~round(max(., na.rm = T), 1)
             )
      ),
      .groups = 'drop')
  
  write.csv(all_daily_inf2na_notimputed_count,
            file.path(tables_output_path,
                      paste0("summary_report_by_completeness_code_for_daily_inf2na_notimputed_",
                             time_period,
                             "_",
                             threshold_label,
                             ".csv")),
            row.names = T)

  # Impute missing values ----
  tic()
  
  qc_daily_summaries_int2_maxgap_imputed <- qc_daily_summaries_int1_pass_completeness %>%
    # * 2 imputation settings for ordinary interpolation----
    imputeTS::na_interpolation(., maxgap = maxgap, option = "linear")
     
  fx_toc(qc_daily_summaries_int2_maxgap_imputed, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  tic()
  qc_daily_summaries_int3_kalman_imputed <- qc_daily_summaries_int2_maxgap_imputed %>%
    # * 3 imputation settings Kalman----
    imputeTS::na_kalman(., maxgap = maxgap + 2)
    ##imputed_data <- impute_station_data(., "YYYY_MM_DD", "station_id")

  fx_toc(qc_daily_summaries_int3_kalman_imputed, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  fx_reportFieldsPar(
    all_daily_summaries_inf2na,
    qc_daily_summaries_int1_pass_completeness,
    qc_daily_summaries_int2_maxgap_imputed,
    qc_daily_summaries_int3_kalman_imputed,
    
      prefix = threshold_label,
    suffix = paste0("maxgap", maxgap),
    output_path = meta_output_path)

  ## Compute Heat Index ----
  tic()
  qc_daily_summaries_imputed_heat_index <- qc_daily_summaries_int3_kalman_imputed %>%
    mutate(relative_humidity_avg = weathermetrics::dewpoint.to.humidity(t = temperature_avg,
                                                               dp = temperature_dewpoint_avg,
                                                               temperature.metric = "celsius")) %>%
    
    mutate(relative_humidity_avg_hum_lib = humidity::RH(t = temperature_avg,
                                                      Td = temperature_dewpoint_avg,
                                                      isK = F)) %>%

    # address observations for which the imputed dew point temperature was higher than the temperature
    # * 4 imputation settings Kalman/NoKalman----
    #imputeTS::na_interpolation(., maxgap = maxgap + 2) %>% #imputeTS::na_kalman(.) %>%
  
    # https://github.com/caijun/humidity (2019)
    # unit is 
    mutate(vapure_pressure_avg = humidity::WVP1(Td = temperature_dewpoint_avg, 
                                                isK = F) * 0.1) %>%
    # convert hectopascal (hPa) to kilopascal (kPa) 
  
    # Apparent Temperature Computation ----
    mutate(apparent_temperature = -2.7 + (1.04 * temperature_avg) +
                                         (2 * vapure_pressure_avg) - 
                                         (0.65 * wind_speed_avg)) %>%

    mutate(diff_avgat_vs_avgt = apparent_temperature - temperature_avg) %>% 
    mutate_if(is.double, ~ round(., 1))

  fx_toc(qc_daily_summaries_imputed_heat_index, 0, threshold_label, paste0(maxgap, "maxgap"))
  

  # Query stations spatial layer to join with the daily data climate data ----
  ### Join summary tables with stations spatial coordinates ----
   stations_daily_summaries_imputed_geo <- qc_daily_summaries_imputed_heat_index %>%
     mutate(YYYY_MM = format(as.Date(YYYY_MM_DD), "%Y-%m")) %>%
     mutate(month_name = lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE))


  # Selected variables -----
  selected_variables <- c(
    "temperature_avg",
    "temperature_dewpoint_avg",
    "wind_speed_avg")


  qc_daily_summaries <- qc_daily_summaries_imputed_heat_index %>%
    group_by(YYYY, station_id) %>%
    summarise(
      n_records = n(),
      n_distinct_days = n_distinct(YYYY_MM_DD),
      across(all_of(selected_variables),
             list(
               null = ~sum(is.na(.)),
               inf = ~sum(is.infinite(.)),
               min = ~round(min(., na.rm = T), 1),
               max = ~round(max(., na.rm = T), 1)
             )
      ),
      .groups = 'drop')

  # << Export compiled stations data ----
  fx_saveCSVPar(
    qc_daily_summaries,
    #stations_daily_summaries_imputed_geo,

    prefix = geography,
    suffix = paste0(time_period, "_",
                    threshold_label, "output_of_imputation_by_transitive_completeness"),

    output_path = reports_path)
  
   fx_saveRObjects(
     stations_daily_summaries_imputed_geo,
     prefix = geography,
     
     suffix = paste0(time_period, "_",
                     threshold_label, "complete"),
     output_path = imputed_outputs_path)
  
   fx_reportFieldsPar(
     qc_daily_summaries,

     stations_daily_summaries_imputed_geo,
  
     prefix = geography,
     suffix = paste0(time_period, "_",
                     threshold_label, "complete"),
  
     output_path = geo_output_path)


   include_cols <- names(select_if(stations_daily_summaries_imputed_geo, is.numeric)) # // . ----[histoboxplot]---- . ----
   params_suffix <- threshold_label
   
   plot_hist_boxplot(stations_daily_summaries_imputed_geo, include_cols, "YYYY", distplot_outputs_path)
   plot_hist_boxplot(stations_daily_summaries_imputed_geo, include_cols, "month_name", distplot_outputs_path)
   
  }, error = function(e){
    
    log_debug(threshold_label)
    log_warn("imputation error")
    log_error(conditionMessage(e))
    
    cat(threshold_label, "ERROR :",conditionMessage(e), "\n")
  })
} # end of for/foreach the loop ----

print(showConnections())
print("Closing")
print(stopCluster(cl))
print(rm(cl))
