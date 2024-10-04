library(tictoc)

params_suffix <- paste0(threshold_label, "_", distribution_set)

plots_output_path <- file.path(runtime_path, paste0("plots_summary_stats_", EHCE_Param))
if (!dir.exists(plots_output_path)) {
  dir.create(file.path(runtime_path, paste0("plots_summary_stats_", EHCE_Param)))
} 

col_list <- c(  "d_temperature_avg",
                "d_temperature_min",
                "d_temperature_max",
                
                "d_relative_humidity_avg",
                "d_relative_humidity_min",
                "d_relative_humidity_max",
                
                "d_wind_speed_avg",
                "d_wind_speed_min",
                "d_wind_speed_max")

vfx_histocomparison(S973_Sheridan_compiled_with_stations_daily_means,
                    S989_T002_EHE_ECE_events_only,
                    col_list,
                    params_output_path,
                    params_suffix)

ehe_ece <- S989_T002_EHE_ECE_events_only

group_vars_universal = c("completeness_code", "EF_distribution_set")
group_vars_annual = c("year_number", "completeness_code", "EF_distribution_set")
group_vars_universal_by_type = c("completeness_code", "EF_distribution_set", "Event_Type")
group_vars_annual_by_type = c("year_number", "completeness_code","EF_distribution_set", "Event_Type")

group_vars_monthly_by_type = c("month_name", "month_number", "completeness_code", "EF_distribution_set", "Event_Type")

summary_vars = c("Event_duration", 
                 "EHF", "ECF",
                 
                 "d_apparent_temperature_avg",
                 "d_apparent_temperature_min",
                 "d_apparent_temperature_max",
                 
                 "d_heat_index_heatstress_lib_avg",
                 "d_heat_index_heatstress_lib_min",
                 "d_heat_index_heatstress_lib_max",
                 
                 "d_wbgt_heatstress_lib_avg",
                 "d_wbgt_heatstress_lib_min",
                 "d_wbgt_heatstress_lib_max",
                 
                 "d_temperature_avg",
                 "d_temperature_min",
                 "d_temperature_max",
                 
                 "d_relative_humidity_avg",
                 "d_relative_humidity_min",
                 "d_relative_humidity_max",
                 
                 "d_vapoure_pressure_avg",
                 "d_vapoure_pressure_min",
                 "d_vapoure_pressure_max",
                 
                 "d_wind_speed_avg",
                 "d_wind_speed_min",
                 "d_wind_speed_max")

ptables_output_path <- file.path(reports_path, paste0("events_summary_stats"))
if (!dir.exists(ptables_output_path)) {
  dir.create(file.path(reports_path, paste0("events_summary_stats")))
} 

meta_ptables_output_path <- file.path(ptables_output_path, paste0("events_summary_stats_", EHCE_Param))
if (!dir.exists(meta_ptables_output_path)) {
  dir.create(file.path(ptables_output_path, paste0("events_summary_stats", EHCE_Param)))
} 

tic()
ehe_ece_u <- events_summary_stats(ehe_ece, group_vars_universal, summary_vars, 
                                  params_suffix, 1, ptables_output_path)

ehe_ece_ut <- events_summary_stats(ehe_ece, group_vars_universal_by_type, summary_vars,  
                                   params_suffix, 1, ptables_output_path)

ehe_ece_a <- events_summary_stats(ehe_ece, group_vars_annual, summary_vars,  
                                  params_suffix, 1, ptables_output_path)

ehe_ece_at <- events_summary_stats(ehe_ece, group_vars_annual_by_type, summary_vars,  
                                   params_suffix, 1, ptables_output_path)

ehe_ece_mt <- events_summary_stats(ehe_ece, group_vars_monthly_by_type, summary_vars,  
                                   params_suffix, 1, ptables_output_path)
toc()

group_vars_event = c("EUID", "completeness_code", "EF_distribution_set", "Event_Type")

col_dict <- list(
  c("Event_duration_avg", "Average Event Duration", "Event_duration_sd"),
  c("Event_duration_max", "Max Event Duration", "Event_duration_sd"),
  
  c("d_temperature_avg_median", "Median Average Temperature", "d_temperature_avg_sd"),
  c("d_temperature_min_median", "Median Min Temperature", "d_temperature_min_sd"),
  c("d_temperature_max_median", "Median Max Temperature", "d_temperature_max_sd"),
  
  c("d_wind_speed_avg_median", "Median Average Wind-speed", "d_wind_speed_avg_sd"),
  c("d_relative_humidity_avg_median", "Median Average Relative", "d_relative_humidity_avg_sd"),
  c("d_vapoure_pressure_avg_median", "Median Average Vapour Pressure", "d_vapoure_pressure_avg_sd"),
  
  c("d_apparent_temperature_avg_median", "Median Average AT", "d_apparent_temperature_avg_sd"),
  c("d_apparent_temperature_min_median", "Median Min AT", "d_apparent_temperature_min_sd"),
  c("d_apparent_temperature_max_median", "Median Max AT", "d_apparent_temperature_max_sd"),
  
  c("d_heat_index_heatstress_lib_avg_median", "Median Average HI", "d_heat_index_heatstress_lib_avg_sd"),
  c("d_heat_index_heatstress_lib_min_median", "Median Min HI", "d_heat_index_heatstress_lib_min_sd"),
  c("d_heat_index_heatstress_lib_max_median", "Median Max HI", "d_heat_index_heatstress_lib_max_sd"),
  
  c("d_wbgt_heatstress_lib_avg_median", "Median Average WBGT", "d_wbgt_heatstress_lib_avg_sd"),
  c("d_wbgt_heatstress_lib_min_median", "Median Min WBGT", "d_wbgt_heatstress_lib_min_sd"),
  c("d_wbgt_heatstress_lib_max_median", "Median Max WBGT", "d_wbgt_heatstress_lib_max_sd"),
  
  c("Distinct_Impacted_Days", "Impacted Days", "Event_duration_sd"),
  c("Distinct_Impacted_Stations", "Impacted Stations", "Event_duration_sd"),
  c("Average_Days_per_Impacted_Station", "Average Days per Impacted Station", "Event_duration_sd"),
  c("Distinct_Events_Universal", "Distinct Events (Universal)", "Event_duration_sd"))
  
summary_vars_y_u = c("Event_duration_max", "Event_duration_avg",
                     "Distinct_Events_Universal", "Distinct_Impacted_Stations")
# Plots -----
map(col_dict, ~fx_lineplot_errbar(ehe_ece_at,
                                  params_output_path,
                                  "year_number",
                                  .x[1],
                                  .x[2],
                                  .x[3],
                                  facet_formula = "completeness_code + EF_distribution_set ~ Event_Type"))

map(col_dict, ~fx_lineplot_errbar(ehe_ece_mt,
                                  params_output_path,
                                  "month_number",
                                  .x[1],
                                  .x[2],
                                  .x[3],
                                  facet_formula = "completeness_code + EF_distribution_set ~ Event_Type"))


fx_lineplot(ehe_ece_at,
            params_output_path,
            "year_number",
            "Distinct_Impacted_Stations",
            "Total Stations Impacted by Extreme Events",
            facet_formula = "cc_label + EF_distribution_set ~ Event_Type")


fx_lineplot(ehe_ece_at,
            params_output_path,
            "year_number",
            "Distinct_Impacted_Days",
            "Total Days Impacted by Extreme Events",
            facet_formula = "cc_label + EF_distribution_set ~ Event_Type")

bar_col_dict <- list(
  c("Distinct_Events_Universal", "Frequency of Extreme Events (Universal)"),
  c("Distinct_Impacted_Stations", "Total Stations Impacted by Extreme Events"))

map(bar_col_dict, ~fx_events_barplot(ehe_ece_at,
                                     params_output_path,
                                     "year_number",
                                     "Year",
                                     .x[1],
                                     .x[2],
                                     facet_formula = "EF_distribution_set ~ Event_Type"))

map(bar_col_dict, ~fx_events_barplot(ehe_ece_mt,
                                     params_output_path,
                                     "month_number",
                                     "Month",
                                     .x[1],
                                     .x[2],
                                     facet_formula = "EF_distribution_set ~ Event_Type"))
