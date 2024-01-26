
dir.create(file.path(sample_output_path, paste0("plots_", i)))
plots_output_path <- file.path(sample_output_path, paste0("plots_", i))

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)
source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

# // *0 [settings] O2 / Year ====
start_year <-  2008L
end_year <-  2022L

gc()

tic()

validation_results_disaggregate_tb <- validation_compiled_test20_delta %>%
  mutate(SquaredError_IDW = (delta_idw ^ 2),
         SquaredError_NN = (delta_nn ^ 2),
         SquaredError_GM = (delta_gridmet ^ 2)) %>%
  mutate(Delta_SE_IDW_NN = SquaredError_IDW - SquaredError_NN,
         Delta_SE_IDW_GM = SquaredError_IDW - SquaredError_GM) %>%
  mutate(Abs_Residuals_IDW = abs(delta_idw),
         Abs_Residuals_NN = abs(delta_idw),
         Abs_Residuals_GM = abs(delta_gridmet)) %>%
  
  mutate(Error_AT_idw_NN_ratio = round((delta_idw / delta_nn), 3)) %>%
  mutate(Error_AT_idw_NN_ratio_advantage = 1 - Error_AT_idw_NN_ratio) %>%
  
  mutate(AE_AT_idw_NN_ratio = round(abs(delta_idw / delta_nn), 3)) %>%
  mutate(AE_AT_idw_NN_ratio_advantage = 1 - AE_AT_idw_NN_ratio) %>%
  
  mutate(AE_AT_idw_GM_ratio = round(abs(delta_idw / delta_gridmet), 3)) %>%
  mutate(AE_AT_idw_GM_ratio_advantage = 1 - AE_AT_idw_GM_ratio) %>% 
  #mutate(params = runtime_params) %>%
  
  mutate(Month = lubridate::month(YYYY_MM_DD)) %>%
  mutate(Season =
           as.character(case_when(
             (Month >= 11) ~ "Cold", #[1, 2, 11, 12]
             (Month <= 9 & Month >= 6) ~ "Heat", #[6, 7, 8, 9]
             (Month <= 2) ~ "Cold",
             TRUE ~ "Moderate"))) #[3, 4, 5, 10]

fx_toc(validation_results_disaggregate_tb, 1, time_period, i)

# Aggregation stats ----
log_debug(paste0("(Sample ", i, ") Computing aggregated stats started"))
tryCatch({
selected_fields <- c(
  "temperature_avg",
  "temperature_avg_62_idw",
  "temperature_avg_nn_idw",
  "temperature_avg_gridmet",
  
  "Abs_Residuals_IDW",
  "Abs_Residuals_NN",
  "Abs_Residuals_GM",
  "SquaredError_IDW",
  "SquaredError_NN",
  "SquaredError_GM"
  )

tic()
validation_results_tb <- validation_results_disaggregate_tb %>% st_drop_geometry() %>% 
  filter_at(vars(selected_fields[1:4]), all_vars(!is.infinite(.))) %>%
  filter_at(vars(selected_fields[1:4]), all_vars(!is.na(.)))
fx_toc(validation_results_tb, 1, time_period, i)
  
ls(validation_results_tb)

## Level 0 aggregation ----
validation_results_all_aggregate <- validation_results_tb %>% 
  group_by(sample)
validation_results_all_aggstats <- fx_validation_summary_stats(df = validation_results_all_aggregate,
                                                             fields = selected_fields,
                                                             prefix = "By_sample__", suffix = i,
                                                             folder = stats_output_path)

## Level 1 aggregation ----
### Level 1 temporal ----
validation_results_y_aggregate <- validation_results_tb %>% 
  group_by(YYYY)
validation_results_y_aggstats <- fx_validation_summary_stats(df = validation_results_y_aggregate,
                                                                     fields = selected_fields,
                                                                     prefix = "By_year__", suffix = i,
                                                                     folder = stats_output_path)

validation_results_m_aggregate <- validation_results_tb %>% 
  group_by(Month)
validation_results_m_aggstats <- fx_validation_summary_stats(df = validation_results_m_aggregate,
                                                             fields = selected_fields,
                                                             prefix = "By_month__", suffix = i,
                                                             folder = stats_output_path)

validation_results_sn_aggregate <- validation_results_tb %>% 
  group_by(Season)
validation_results_sn_aggstats <- fx_validation_summary_stats(df = validation_results_sn_aggregate,
                                                                    fields = selected_fields,
                                                                    prefix = "By_season__", suffix = i,
                                                                    folder = stats_output_path)

### Level 1 spatial ----
tryCatch({
tic()
validation_results_st_aggregate <- validation_results_tb %>% 
  group_by(station_id)
validation_results_st_aggstats <- fx_validation_summary_stats(df = validation_results_st_aggregate,
                                                             fields = selected_fields,
                                                             prefix = "By_station__", suffix = i,
                                                             folder = stats_output_path)
fx_toc(validation_results_st_aggstats, 1, time_period, i)
log_success("Aggregation stats completed")
}, error = function(e){
  log_warn("Station based stats failed")
  log_error(conditionMessage(e))
})

validation_results_s_aggregate <- validation_results_tb %>% 
  group_by(state)
validation_results_s_aggstats <- fx_validation_summary_stats(df = validation_results_s_aggregate,
                                                              fields = selected_fields,
                                                              prefix = "By_state__", suffix = i,
                                                              folder = stats_output_path)

validation_results_c_aggregate <- validation_results_tb %>% 
  group_by(Climate_Region)
validation_results_c_aggstats <- fx_validation_summary_stats(df = validation_results_c_aggregate,
                                                                     fields = selected_fields,
                                                                     prefix = "By_climateregion__", suffix = i,
                                                                     folder = stats_output_path)

## Level 2 aggregation ----
validation_results_ys_aggregate <- validation_results_tb %>%
  group_by(YYYY, state)
validation_results_ys_aggstats <- fx_validation_summary_stats(df = validation_results_ys_aggregate,
                                                                     fields = selected_fields,
                                                                     prefix = "By_year_and_state__", suffix = i,
                                                                     folder = stats_output_path)

validation_results_yc_aggregate <- validation_results_tb %>% 
  group_by(YYYY, Climate_Region)
validation_results_yc_aggstats <- fx_validation_summary_stats(df = validation_results_yc_aggregate,
                                                     fields = selected_fields,
                                                     prefix = "By_year_and_climateregion__", suffix = i,
                                                     folder = stats_output_path)

validation_results_ms_aggregate <- validation_results_tb %>%
  group_by(Month, state)
validation_results_ms_aggstats <- fx_validation_summary_stats(df = validation_results_ms_aggregate,
                                                              fields = selected_fields,
                                                              prefix = "By_month_and_state__", suffix = i,
                                                              folder = stats_output_path)

validation_results_mc_aggregate <- validation_results_tb %>% 
  group_by(Month, Climate_Region)
validation_results_mc_aggstats <- fx_validation_summary_stats(df = validation_results_mc_aggregate,
                                                              fields = selected_fields,
                                                              prefix = "By_month_and_climateregion__", suffix = i,
                                                              folder = stats_output_path)

validation_results_sc_aggregate <- validation_results_tb %>%
  group_by(Season, Climate_Region)
validation_results_sc_aggstats <- fx_validation_summary_stats(df = validation_results_sc_aggregate,
                                                     fields = selected_fields,
                                                     prefix = "By_season_and_climateregion__", suffix = i,
                                                     folder = stats_output_path)

validation_results_ysn_aggregate <- validation_results_tb %>% 
  group_by(YYYY, Season)
validation_results_ysn_aggstats <- fx_validation_summary_stats(df = validation_results_ysn_aggregate,
                                                                     fields = selected_fields,
                                                                     prefix = "By_year_and_season__", suffix = i,
                                                                     folder = stats_output_path)

validation_results_sns_aggregate <- validation_results_tb %>% 
  group_by(Season, state)
validation_results_sns_aggstats <- fx_validation_summary_stats(df = validation_results_sns_aggregate,
                                                                      fields = selected_fields,
                                                                      prefix = "By_season_and_state__", suffix = i,
                                                                      folder = stats_output_path)

## >>> Write cumulative tables ----
sample_stats_filename <- file.path(runtime_path,
                            paste0(time_period, "_aggregate_",
                              "stats_sample_all", ".csv"))
write.table(validation_results_all_aggstats,
            sample_stats_filename, sep = ",",
            row.names = F, 
            col.names = !file.exists(sample_stats_filename),
            append = T)

## >>> Export results of aggregation ----
fx_saveRObjects(
  validation_results_disaggregate_tb,
  validation_results_all_aggstats,
  
  validation_results_y_aggstats,
  validation_results_m_aggstats,
  validation_results_sn_aggstats,
  validation_results_s_aggstats,
  validation_results_c_aggstats,
  
  validation_results_yc_aggstats,
  validation_results_sc_aggstats,
  validation_results_ys_aggstats,
  validation_results_ms_aggregate,
  validation_results_mc_aggregate,
  validation_results_ysn_aggstats,
  validation_results_sns_aggstats,
  
  validation_results_st_aggstats,
  
  prefix = time_period,
  suffix = paste0("_sample_", i),
  output_path = sample_output_path)

fx_reportFieldsPar(
  validation_results_disaggregate_tb,
  validation_results_all_aggstats,
  
  validation_results_y_aggstats,
  validation_results_m_aggstats,
  validation_results_sn_aggstats,
  validation_results_s_aggstats,
  validation_results_c_aggstats,
  
  validation_results_yc_aggstats,
  validation_results_sc_aggstats,
  validation_results_ys_aggstats,
  validation_results_ms_aggregate,
  validation_results_mc_aggregate,
  validation_results_ysn_aggstats,
  validation_results_sns_aggstats,
  
  validation_results_st_aggstats,
  
  prefix = time_period,
  suffix = paste0("_sample_", i),
  output_path = sample_output_path)

  log_success("Aggregation stats completed")
}, error = function(e){
  log_warn("Aggregation stats failed")
  log_error(conditionMessage(e))
})

# Visualization ----
log_debug(paste0("(Sample ", i, ") Visualizing distributions started"))
tic()
validation_results_long_table <- pivot_longer(validation_compiled_test20_delta,
                                              cols=c(delta_nn,
                                                     delta_idw,
                                                     delta_gridmet),
                                              names_to = "Method",
                                              values_to = "Delta_AT") %>% 
  mutate(month_number = lubridate::month(format(YYYY_MM_DD, "%Y-%m-%d")),
         month_name = lubridate::month(format(YYYY_MM_DD, "%Y-%m-%d"), label = TRUE))
fx_toc(validation_results_long_table, 1, time_period, i)

#rm(validation_compiled_test20_delta)

#ls(validation_results_long_table)
## Boxplots ----
tryCatch({
  map("Overall", ~ {
    category_df <- validation_results_long_table
    print(.x)
    print(dim(category_df))
    fx_delta_boxplot(category_df, "Delta_AT", "Aggregation",
                     .x, i, plots_output_path)
  })
  
  map(1:12, ~ {
    category_df <- validation_results_long_table %>% dplyr::filter(month_number == .x)
    print(.x)
    print(dim(category_df))
    fx_delta_boxplot(category_df, "Delta_AT", "Month",
                     .x, i, plots_output_path)
  })
  
  map(end_year:start_year, ~ {
    category_df <- validation_results_long_table %>% dplyr::filter(YYYY == .x)
    print(.x)
    print(dim(category_df))
    fx_delta_boxplot(category_df, "Delta_AT", "Year",
                     .x, i, plots_output_path)
  })
  log_success("Boxplots completed")
}, error = function(e){
  log_warn("Boxplot")
  log_error(conditionMessage(e))
})

## Densityplots ----
tryCatch({
  map("Overall", ~ {
    category_df <- validation_results_long_table
    print(.x)
    print(dim(category_df))
    fx_delta_densityplot(category_df, "Delta_AT", "Aggregation",
                         .x, i, plots_output_path)
  })
  
  map(1:12, ~ {
    category_df <- validation_results_long_table %>% dplyr::filter(month_number == .x)
    print(.x)
    print(dim(category_df))
    fx_delta_densityplot(category_df, "Delta_AT", "Month",
                         .x, i, plots_output_path)
  })
  
  map(end_year:start_year, ~ {
    category_df <- validation_results_long_table %>% dplyr::filter(YYYY == .x)
    print(.x)
    print(dim(category_df))
    fx_delta_densityplot(category_df, "Delta_AT", "Year",
                         .x, i, plots_output_path)
  })
  log_success("Densityplots completed")
}, error = function(e){
  log_warn("Densityplot")
  log_error(conditionMessage(e))
})


variables_to_plot <- c(
                       "temperature_avg_62_idw",
                       "temperature_avg_nn_idw",
                       "temperature_avg_gridmet",
                       "temperature_avg")

## Scatterplots ----
tryCatch({

# # Use purrr::map to iterate over both years and variables
map(variables_to_plot, ~ {
    variable <- .x
    print(variable)

    map("Overall", ~ {
      category_df <- validation_results_long_table
      print(.x)
      print(dim(category_df))
      fx_r2_plot(category_df, "temperature_avg", variable,
                 "Aggregation",
                 .x, i, plots_output_path)
    })

    map(1:12, ~ {
      category_df <- validation_results_long_table %>% dplyr::filter(month_number == .x)
      print(.x)
      print(dim(category_df))
      fx_r2_plot(category_df, "temperature_avg", variable,
                 "Month",
                 .x, i, plots_output_path)
    })
    
    map(end_year:start_year, ~ {
      category_df <- validation_results_long_table %>% dplyr::filter(YYYY == .x)
      print(.x)
      print(dim(category_df))
      fx_r2_plot(category_df, "temperature_avg", variable,
                 "Year",
                 .x, i, plots_output_path)
    })
    
  })
  log_success("Scatterplots completed")
}, error = function(e){
  log_warn("Scatterplot")
  log_error(conditionMessage(e))
})


log_success(paste0("(Sample ", i, ") Data processing completed!"))
