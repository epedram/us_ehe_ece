# Subset the combined daily data set based on completeness criteria ----
 # {{ + initiate the temporal loop + }} ----
library(doParallel)
#cores <- 4 # detectCores() - 2
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

# for(i in c(6, 4, 2, 0)){
#   i <- 6
  library(here)
  library(dplyr)
  library(data.table)
  library(logger)
  #library(sf)
  library(imputeTS)
  ## https://www.rdocumentation.org/packages/imputeTS/
  #library(weathermetrics)
  ## https://github.com/geanders/weathermetrics
  ## https://rdrr.io/cran/weathermetrics/man/dewpoint.to.humidity.html
  ## https://www.wpc.ncep.noaa.gov/html/heatindex.shtml

  library(HeatStress)
  #HeatStress::indexShow()
  #https://github.com/anacv/HeatStress/

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
  #sapply(2:5, function(i) {
  #sapply(1:max(all_daily_summaries_inf2na$completeness_code), function(i) {
  #  i = 5

  qc_annual_summaries <- all_annual_summaries %>%
    dplyr::filter(completeness_code >= i)
  glimpse(qc_annual_summaries)

  qc_daily_summaries_int1_pass_completeness <- all_daily_summaries_inf2na %>%
    dplyr::filter(completeness_code >= i) %>% 
    arrange(station_id, YYYY_MM_DD) #%>% 
    #head(200000) # // ===== [1:100000 subsetting] ====
    #group_by(station_id)
  #glimpse(qc_daily_summaries_int1_pass_completeness)
  
  length(unique(qc_daily_summaries_int1_pass_completeness$station_id))

  all_daily_inf2na_notimputed_count <- qc_daily_summaries_int1_pass_completeness %>%
    #mutate(completeness_percent = 100-((maxcode - completeness_code)*5)) %>%
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

  ## Vis1: missing values gapsize and number of stations ----
  #source(here::here("data_preparation",
  #                 "0503_vis_na_gap_size_stations_barplot.R"), local=T)

 #imputeTS::statsNA(all_daily_summaries_inf2na$temperature_avg)

  # Impute missing values ----
  tic()
  #ls(qc_daily_summaries_int1_pass_completeness)
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
  
  #qc_daily_summaries_int4_unlimited_kalman <- qc_daily_summaries_int2_maxgap_imputed %>%
  #imputeTS::na_kalman(.) # for testing purpose
  
  fx_reportFieldsPar(
    all_daily_summaries_inf2na,
    qc_daily_summaries_int1_pass_completeness,
    qc_daily_summaries_int2_maxgap_imputed,
    qc_daily_summaries_int3_kalman_imputed,
    
      prefix = threshold_label,
    suffix = paste0("maxgap", maxgap),
    output_path = meta_output_path)
  
  # fx_saveCSVPar(
  #   qc_daily_summaries_int1_pass_completeness,
  #   qc_daily_summaries_int2_maxgap_imputed,
  #   qc_daily_summaries_int3_kalman_imputed,
  #   
  #   prefix = threshold_label,
  #   suffix = paste0("maxgap", maxgap),
  #   output_path = geo_output_path)
  
  # qc_daily_summaries_imputed <- qc_daily_summaries_int3_kalman_imputed
  # 
  # glimpse(qc_daily_summaries_imputed)
  #"all_daily_summaries_inf2na",
  #"qc_daily_summaries_int1_pass_completeness",
  #"qc_daily_summaries_int2_maxgap_imputed",
  #"qc_daily_summaries_int3_kalman_imputed"
  #qc_daily_summaries_int4_unlimited_kalman
  # sum(is.na(qc_daily_summaries_imputed$temperature_avg))
  # sum(is.infinite(qc_daily_summaries_imputed$temperature_dewpoint_avg))
  # hist(qc_daily_summaries_imputed$wind_speed_avg)
#indexShow()

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

    # https://github.com/anacv/HeatStress
    mutate(apparent_temperature_heatstress_lib = HeatStress::apparentTemp(
        tas = temperature_avg,
        hurs = relative_humidity_avg,
        wind = wind_speed_avg)) %>%
    
    mutate(affective_temperature_heatstress_lib = HeatStress::effectiveTemp(
      tas = temperature_avg,
      hurs = relative_humidity_avg,
      wind = wind_speed_avg)) %>%
    
    # mutate(wbgt_heatstress_lib = round(HeatStress::wbgt.Bernard(
    #   tas = temperature_avg,
    #   dewp = temperature_dewpoint_avg,
    #   noNAs = TRUE,
    #   swap = FALSE), 1)) %>%
    
    mutate(heat_index_heatstress_lib = HeatStress::hi(
      tas = temperature_avg,
      hurs = relative_humidity_avg)) %>%
    
    # https://github.com/geanders/weathermetrics (2017)
    mutate(heat_index_weathermetrics_lib = weathermetrics::heat.index(
        t = temperature_avg,
        dp = temperature_dewpoint_avg,
        temperature.metric = "celsius")) %>%
  
    # Apparent Temperature Computation ----
    mutate(apparent_temperature = -2.7 + (1.04 * temperature_avg) +
                                         (2 * vapure_pressure_avg) - 
                                         (0.65 * wind_speed_avg)) %>%

    mutate(diff_avgat_vs_avgt = apparent_temperature - temperature_avg) %>% 
    mutate_if(is.double, ~ round(., 1))
    #mutate(diff_humidity = relative_humidity_avg_WMetrics - relative_humidity_avg)
  
  fx_toc(qc_daily_summaries_imputed_heat_index, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  #hist(qc_daily_summaries_imputed_heat_index$diff_avgat_vs_avgt)
  #hist(qc_daily_summaries_imputed_heat_index$diff_humidity)
  #mutate(diff2 = apparent_temperature_test - temperature_avg) %>%
  #mutate(diff3 = apparent_temperature - apparent_temperature_test)
  #dim(all_daily_summaries_inf2na)
  # sum(is.na(qc_daily_summaries_imputed$temperature_avg))
  # sum(is.na(qc_daily_summaries_imputed$temperature_dewpoint_avg))
  # sum(is.na(qc_daily_summaries_imputed$wind_speed_avg))


  #hist(qc_daily_summaries_imputed_heat_index$vapure_pressure)
  #hist(qc_daily_summaries_imputed_heat_index$diff2)
  #hist(qc_daily_summaries_imputed_heat_index$diff3)

  #boxplot(qc_daily_summaries_imputed_heat_index$diff1)

  #hist(qc_daily_summaries_imputed_heat_index$relative_humidity_max)
  #hist(qc_daily_summaries_imputed_heat_index$heat_index)
  #hist(qc_daily_summaries_imputed_heat_index$apparent_temperature)
  # dim(all_daily_summaries_inf2na)
  # dim(qc_daily_summaries)
  # sum(is.na(qc_daily_summaries$temperature_avg))
  # sum(is.na(qc_daily_summaries$temperature_dewpoint_avg))
  # sum(is.na(qc_daily_summaries$wind_speed_avg))


  # sum(is.na(qc_daily_summaries_imputed_heat_index$heat_index))
  # sum(is.na(qc_daily_summaries_imputed_heat_index$apparent_temperature))
  # sum(is.na(qc_daily_summaries_imputed_heat_index$relative_humidity_max))
  # sum(is.infinite(qc_daily_summaries_imputed_heat_index$apparent_temperature))

  # Query stations spatial layer to join with the daily data climate data ----
  ### Join summary tables with stations spatial coordinates ----
   stations_daily_summaries_imputed_geo <- qc_daily_summaries_imputed_heat_index %>%
  #   merge(.,
  #         noaa_isd_2021_sf_ca_state_100km_simplified,
  #         by.x="station_id",
  #         by.y="station_id",
  #         all.x = TRUE,
  #         suffix = c("","_sp")) %>%
     mutate(YYYY_MM = format(as.Date(YYYY_MM_DD), "%Y-%m")) %>%
     mutate(month_name = lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE))

  # stations_annual_summaries_geo <- qc_annual_summaries %>%
  #   merge(.,
  #         noaa_isd_2021_sf_ca_state_100km_simplified,
  #         by.x="station_id",
  #         by.y="station_id",
  #         all.x = TRUE,
  #         suffix = c("","_web_api"))

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

  #  fx_saveCSVPar(
  # #   stations_annual_summaries_geo,
  #    stations_daily_summaries_imputed_geo,
  # #
  #    prefix = geography,
  #    suffix = paste0(year, "_",
  #                    threshold_label, "complete"),
  #    output_path = output_dir)
  #
  # RObject_Files_path <- file.path(output_dir, "RObject_Files")
  #
   fx_saveRObjects(
     stations_daily_summaries_imputed_geo,
     prefix = geography,
     
     suffix = paste0(time_period, "_",
                     threshold_label, "complete"),
     output_path = imputed_outputs_path)
  #
   fx_reportFieldsPar(
     qc_daily_summaries,
  #   stations_annual_summaries_geo,
     stations_daily_summaries_imputed_geo,
  #
     prefix = geography,
     suffix = paste0(time_period, "_",
                     threshold_label, "complete"),
  #
     output_path = geo_output_path)

#   v_missing3 <- ggplot_na_distribution(stations_daily_summaries_imputed_geo$apparent_temperature,
#                                         title = paste0("Variable: Apparent Temperature - (",
#                                                                 threshold_label,
#                                                                 ")"))
#   v_missing_plot_fn3 <- paste0(meta_output_path, "/",
#                                "na_distribution_at_",
#
#                                "_",
#                                threshold_label,
#                                ".jpg")
#
#   ggsave(v_missing_plot_fn3,
#          plot = v_missing3, dpi = 200,
#          width = 32, height = 16, units = "cm")

  #glimpse(stations_daily_summary_compiled_geo)
  #glimpse(stations_annual_summary_compiled_geo)
   
   include_cols <- names(select_if(stations_daily_summaries_imputed_geo, is.numeric)) # // . ----[histoboxplot]---- . ----
   params_suffix <- threshold_label
   
   plot_hist_boxplot(stations_daily_summaries_imputed_geo, include_cols, "YYYY", distplot_outputs_path)
   plot_hist_boxplot(stations_daily_summaries_imputed_geo, include_cols, "month_name", distplot_outputs_path)
   
  #}) # end of sapply the loop ----
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
