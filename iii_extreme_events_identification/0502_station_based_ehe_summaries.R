# Compute station-based EHE statistical summaries----
## Estimate impacted population based on the nearest station to census blocks----
library(sjmisc)


# compute station based extreme events summaries ----
selected_variables <- c(
  "EHE_duration",
  "DM_AT",
  "temperature_avg",
  "wind_speed_avg",
  "temperature_dewpoint_avg",
  "relative_humidity_avg",
  "EHMI"
  )

##```{r}
CA_EHE_Summary <- ehe_ece %>%

  st_drop_geometry() %>%

  summarise(
    events_records = n(),
    unique_impacted_stations = n_distinct(station_id),
    n_distinct_days_n = n_distinct(DATE, na.rm = T),
    n_distinct_events_n = n_distinct(EUID, na.rm = T),

        across(all_of(selected_variables),
           list(
             null = ~sum(is.na(.)),
             inf = ~sum(is.infinite(.)),
             avg = ~round(mean(as.double(.), na.rm = T), 1),
             min = ~round(min(as.double(.), na.rm = T), 1),
             max = ~round(max(as.double(.), na.rm = T), 1),
             median = ~median(as.double(.), na.rm = T),
             sd = ~round(sd(as.double(.), na.rm = T), 1),
             #se = ~round((sd(as.double(.), na.rm = T)/sqrt(n(.))), 1),
             #se2 = ~round(sd/events_records, 1),
             Q1 = ~round(quantile(., probs = .25, na.rm = TRUE), 1),
             Q3 = ~round(quantile(., probs = .75, na.rm = TRUE), 1)
           )
          ),
    .groups = 'drop') %>%
  mutate(YYYY = start_year) %>%
  mutate(
         EHE_duration_SE = round((EHE_duration_sd / sqrt(events_records)), 2),
         temperature_avg_SE = round((temperature_avg_sd / sqrt(events_records)), 2),
         DM_AT_SE = round((DM_AT_sd / sqrt(events_records)), 2)
         #temperature_avg_SE = round((temperature_avg_sd / events_records), 1),
         )

# fx_saveCSV(CA_EHE_Summary, output_path,
#            prefix = "00_Aggregate_stats",
#            suffix = year)

CA_EHE_Summary_rotated <- rotate_df(CA_EHE_Summary)

fx_saveCSV(CA_EHE_Summary_rotated, tables_output_path,
           prefix = "00_Aggregate_stats",
           suffix = year)

# Annual ----
CA_EHE_Summary_2 <- ehe_ece %>%
  st_drop_geometry() %>%
  filter(Event_Type == "Extreme Heat Event") %>%
    group_by(station_id) %>%

  summarise(
    #unique_stations_x_days_combined = n(),
    n_distinct_days_n = n_distinct(DATE, na.rm = TRUE),
    cumulative_impacted_censusgeo_n = n_distinct(station_id, na.rm = TRUE),
    avg_impacted_censusgeo_perday = round(mean(cumulative_impacted_censusgeo_n/n_distinct_days_n,
                                              na.rm = TRUE), 3),
    n_distinct_events_n = n_distinct(EUID, na.rm = TRUE),

    across(all_of(selected_variables),
           list(
             avg = ~round(mean(as.double(.), na.rm = T), 1),
             min = ~round(min(as.double(.), na.rm = T), 1),
             max = ~round(max(as.double(.), na.rm = T), 1),
             median = ~median(as.double(.), na.rm = T),
             sd = ~round(sd(as.double(.), na.rm = T), 1),
             #se = ~round(sd(as.double(.), na.rm = T)/n(.), 1),
             Q1 = ~round(quantile(., probs = .25, na.rm = TRUE), 1),
             Q3 = ~round(quantile(., probs = .75, na.rm = TRUE), 1)
           )
    ),
    .groups = 'drop'
  ) %>%
  mutate(YYYY = start_year)

fx_saveCSV(CA_EHE_Summary_2,
             tables_output_path,
             prefix = "01_Stations_aggregate_stats",
             suffix = year)
#ls(ehe_ece)
CA_EHE_Summary_annual <- ehe_ece %>%
  group_by(YYYY) %>% 
  st_drop_geometry() %>%
  
  summarise(
    events_records = n(),
    unique_impacted_stations = n_distinct(station_id),
    n_distinct_days_n = n_distinct(DATE, na.rm = T),
    n_distinct_events_n = n_distinct(EUID, na.rm = T),
    
    across(all_of(selected_variables),
           list(
             null = ~sum(is.na(.)),
             inf = ~sum(is.infinite(.)),
             avg = ~round(mean(as.double(.), na.rm = T), 1),
             min = ~round(min(as.double(.), na.rm = T), 1),
             max = ~round(max(as.double(.), na.rm = T), 1),
             median = ~median(as.double(.), na.rm = T),
             sd = ~round(sd(as.double(.), na.rm = T), 1),
             #se = ~round((sd(as.double(.), na.rm = T)/sqrt(n(.))), 1),
             #se2 = ~round(sd/events_records, 1),
             Q1 = ~round(quantile(., probs = .25, na.rm = TRUE), 1),
             Q3 = ~round(quantile(., probs = .75, na.rm = TRUE), 1)
           )
    ),
    .groups = 'drop') %>%
  mutate(YYYY = start_year) %>%
  mutate(
    EHE_duration_SE = round((EHE_duration_sd / sqrt(events_records)), 2),
    temperature_avg_SE = round((temperature_avg_sd / sqrt(events_records)), 2),
    DM_AT_SE = round((DM_AT_sd / sqrt(events_records)), 2)
    #temperature_avg_SE = round((temperature_avg_sd / events_records), 1),
  )
CA_EHE_Summary_annual
#
# ### Monthly boxplot ----
multipanel_boxplots <-
  ggplot(ehe_ece %>% st_drop_geometry(),
         aes(x = EHE_duration,
             y = month_name,
             group = month_name)) +
  #geom_violin() +
  geom_boxplot()+

  guides(x =  guide_axis(angle = 0)) +

  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Events Duration",
       y = "Month") +
  labs(title = paste0("Monthly Distribution of Extreme Heat Events at Stations"
                      #"State of California ",
                      )) +
  labs(subtitle = paste0("State of California ",
                         year
  )) + theme_classic() +
  scale_x_continuous(
    breaks = (1:12))


plot_file_name <- paste0(plots_output_path,
                         "/EHE_",
                         "Boxplot_by_",
                         "Month",
                         ".jpg")

ggsave(plot_file_name,
       plot = multipanel_boxplots,
       dpi = 300,
       width = 22, height = 18, units = "cm")

plot_file_name2 <- paste0(plots_output_path,
                         "/EHE_",
                         "Boxplot_jitter_by_",
                         "Month",
                         ".jpg")

ggsave(plot_file_name2,
       plot = multipanel_boxplots + geom_jitter(),
       dpi = 300,
       width = 22, height = 18, units = "cm")


###


fx_appendCSV(CA_EHE_Summary,
             scratch_dir,
             prefix = "00_aggregate_stats",
             suffix = "study_period")

