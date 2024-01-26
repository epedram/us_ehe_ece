library(tictoc)
params_suffix <- paste0(threshold_label, "_", distribution_set)

glimpse(EHE_ECE_compiled)

col_list <- c("apparent_temperature", 
              "temperature_avg",
              "wind_speed_avg",
              "temperature_dewpoint_avg",
              "relative_humidity_avg",
              "vapure_pressure_avg")

vfx_histocomparison(stations_daily_means_compiled,
                    EHE_ECE_compiled,
                    col_list,
                    params_output_path,
                    params_suffix)

ehe_ece <- EHE_ECE_events_only
ls(ehe_ece)

group_vars_universal = c("completeness_code", "EF_distribution_set")
group_vars_annual = c("year_number", "completeness_code", "EF_distribution_set")
group_vars_universal_by_type = c("completeness_code", "EF_distribution_set", "Event_Type")
group_vars_annual_by_type = c("year_number", "completeness_code","EF_distribution_set", "Event_Type")

group_vars_monthly_by_type = c("month_name", "month_number", "completeness_code", "EF_distribution_set", "Event_Type")

summary_vars = c("Event_duration", "DM_AT", "EHF", "ECF",
                 "relative_humidity_avg", "temperature_avg", "wind_speed_avg")

tic()
ehe_ece_u <- events_summary_stats(ehe_ece, group_vars_universal, summary_vars, params_suffix, 1)
ehe_ece_ut <- events_summary_stats(ehe_ece, group_vars_universal_by_type, summary_vars, params_suffix, 1)
ehe_ece_a <- events_summary_stats(ehe_ece, group_vars_annual, summary_vars, params_suffix, 1)
ehe_ece_at <- events_summary_stats(ehe_ece, group_vars_annual_by_type, summary_vars, params_suffix, 1)
ehe_ece_mt <- events_summary_stats(ehe_ece, group_vars_monthly_by_type, summary_vars, params_suffix, 1)
toc()

#ehe_ece_e <- events_summary_stats(ehe_ece, group_vars_event, summary_vars, 1)
fx_reportFieldsPar(
  ehe_ece_a,
  ehe_ece_u,
  ehe_ece_ut,
  ehe_ece_at,
  ehe_ece_mt,
  
  suffix = st_label,
  output_path = stats_outputs_path)

group_vars_event = c("EUID", "completeness_code", "EF_distribution_set", "Event_Type")

col_dict <- list(
  c("Event_duration_avg", "Average Event Duration", "Event_duration_sd"),
  c("Event_duration_max", "Max Event Duration", "Event_duration_sd"),
  
  c("Distinct_Impacted_Days", "Impacted Days", "Event_duration_sd"),
  c("Distinct_Impacted_Stations", "Impacted Stations", "Event_duration_sd"),
  c("Average_Days_per_Impacted_Station", "Average Days per Impacted Station", "Event_duration_sd"),
  
  c("Distinct_Events_Universal", "Distinct Events (Universal)", "Event_duration_sd"),
  c("temperature_avg_median", "Median Average Temperature", "temperature_avg_sd"),
  c("wind_speed_avg_median", "Median Average Wind-speed", "wind_speed_avg_sd"),
  c("relative_humidity_avg_median", "Median Average Relative", "relative_humidity_avg_sd"),
  
  c("DM_AT_median", "Median AT", "DM_AT_sd"))


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
  c("Distinct_Impacted_Stations", "Total Stations Impacted by Extreme Events")
  #c("Distinct_Events_Stations", "Frequency of Extreme Events factored by Impacted Stations")
)

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

ls(EHE_ECE_events_only)

fx_histboxplot(EHE_ECE_events_only, "month_name", 
                  params_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
fx_histboxplot(EHE_ECE_events_only, "year_number", 
                  params_output_path, params_suffix)

rm(EHE_ECE_events_only)

