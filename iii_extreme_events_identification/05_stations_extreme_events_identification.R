## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

library(here)
library(furrr)
library(purrr)
library(tictoc)
library(getopt)
library(tictoc)
library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(logger)

# Set up computational parameters ----
spatial_projection  = 4269 #NAD83
#plots dimensions
ww = 22 #width
hh = 28 #height

start_year <-  2008
end_year <-  2022

spec = matrix(c(
  #'state', 's', 1, "character",
  #'year'  , 'y', 1, "character",
  'cellsize', 'c', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)

opt = getopt(spec)

cores <- as.integer(opt$cores)
#cores <- 3 # // [runtime settings] O2 / Cores ==== 

job_id <- opt$job_id
pointer <- "archiveX"

project_title <- "05_noaa_isd_imputed_35"
task_title <- "ehf_ecf_ehmi_ehci_ehe_ece"
runtime_params <- "sorted_stats"

geography <- paste("us", sep = "_")

time_period <- paste(start_year, end_year, sep = "_")

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

print(Sys.time())

code_labels <- c("65" = "All stations (short gaps imputed)",
                 "75" = "75% completeness (short gaps imputed)",
                 "85" = "85% completeness (short gaps imputed)",
                 "95" = "95% completeness (short gaps imputed)")

# Computational script ----

library(doParallel)
# 
# # # create a cluster object
# cl <- makeCluster(cores, outfile = file.path(runtime_path, paste0("foreach_logs_",
#                                                                   time_period,
#                                                                   ".txt")))
# # register the cluster
# registerDoParallel(cl)
# 
# print(cl)
# 
# #i <- c(65, 70, 75, 85)
# # * 1 ----
# foreach(i = c(65, 75, 85, 95),
#         .multicombine = TRUE,
#         .verbose = TRUE,
#         .packages =c("here", "tictoc", "data.table",
#                      "sf", "janitor", "tidyverse")
#         ) %dopar% {

### I/O -----
inputs_path <- file.path(source_dir, "03_imputed_daily_values_7classes_sorted_us_49")

us_noaa_isd_stations <- readRDS(file.path(source_dir,
                                          "00_noaa_stations_data_catalog_2007_2022",
                                          "us49_stations_2007_2022.rds")) %>%
  dplyr::select(!YYYY)

states_file_path <- file.path(source_dir,
                              "00_base_spatial_layers_grids_census_admin_boundaries", 
                              "us49_states_geo_tigris.rds")
states_geo <- readRDS(states_file_path)[[1]] %>% 
              dplyr::select(contains("STUSPS") | contains("Climate_Region"))

#plot(states_geo["Climate_Region"])
glimpse(states_geo)

noaa_stations_sp <- us_noaa_isd_stations %>% st_drop_geometry() %>% 
  distinct(station_id, .keep_all = TRUE) %>% 
  merge(.,
        states_geo %>% st_drop_geometry(),
        by.x="state",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp")) %>% 
  janitor::clean_names()

glimpse(noaa_stations_sp)

for(i in c(65, 75, 85)){
  # {{ + initiate the completeness loop + }} ----
library(here)
library(tidyverse)
library(data.table)
library(janitor)
library(tictoc)
library(sf)
  
fx_io()

  threshold_label <- as.character(i) #(100-((7-i)*5))
  print(threshold_label)
  
  # Logger set up ----
  logger_file <- file.path(runtime_path, paste0(time_period,
                                                "_logger_outputs_for_sample_",
                                                threshold_label,
                                                ".log"))
  ## define the file logger with log rotation enabled
  log_appender(appender_file(logger_file, max_files = 1L,
                             max_lines = Inf, max_bytes = Inf))
  
  log_threshold(TRACE)
  log_threshold()

# * 2 ----
#plan(multisession, workers = 4)

#future_map(i, function(i) {
#for(i in c(95, 85, 73, 70)){
#for(i in c(75, 70)){  

 stations_daily_summary_compiled <- readRDS(file.path(inputs_path,
                                                      "imputed_daily_data", 
                                                      paste0("us_49_stations_daily_summaries_imputed_geo_2007_2022_",
                                                            threshold_label,
                                                            "complete.rds")))[[1]]
 glimpse(stations_daily_summary_compiled)
 
 # stations_daily_summary_compiled <- merge(stations_daily_summary_compiled[1:200000],  #// [1:12] ----
 #                                          noaa_stations_sp[c("station_id", "Climate_Region")],
 #                                          by.x="station_id",
 #                                          by.y="station_id",
 #                                          all.x = TRUE,
 #                                          suffix = c("","_sp")) %>%
 stations_daily_summary_compiled <- stations_daily_summary_compiled %>%  #[1:200000] %>% #// [1:12] ----
          mutate(geography = "contiguous_us") 
 
 glimpse(stations_daily_summary_compiled)
 
  dir.create(file.path(runtime_path, paste0(threshold_label, "_percent_complete")))
  params_output_path <- file.path(runtime_path, paste0(threshold_label, "_percent_complete"))
  
  dir.create(file.path(params_output_path, paste0("summary_stats")))
  stats_outputs_path <- file.path(params_output_path, paste0("summary_stats"))

# test stations data completeness -----
selected_variables <- c(
  "temperature_avg",
  "temperature_dewpoint_avg",
  "wind_speed_avg")

 
qc_daily_aggregated <- stations_daily_summary_compiled %>%
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
  qc_daily_aggregated,

  suffix = paste(threshold_label, "complete", sep = "_"),
  prefix = st_label,
  output_path = tables_output_path)

states_file_path <- file.path(source_dir,
                              "00_base_spatial_layers_grids_census_admin_boundaries", 
                              "us49_states_geo_tigris.rds")
states_geo <- readRDS(states_file_path)[[1]]

states_geo_climate <- states_geo %>% st_drop_geometry() %>% 
  dplyr::select(contains("STUSPS") | contains("Climate_Region"))

log_debug(paste0("(Criteria ", threshold_label, ") Computing aggregated stats started"))

tryCatch({
stations_geo_path <- file.path(source_dir,
                               "00_noaa_stations_data_catalog_2007_2022",
                               "us49_stations_2007_2022.rds")
stations_geo <- readRDS(stations_geo_path) %>%
  st_transform(spatial_projection) %>% 
  dplyr::select(!YYYY) %>% 
  distinct(station_name, .keep_all = TRUE) %>% 
  merge(.,
        states_geo_climate,
        by.x="state",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp")) %>% 
  dplyr::select(contains("state") |
                  contains("station_id") | 
                  contains("Climate_Region"))

stations_daily_summary_compiled_lite <- stations_daily_summary_compiled %>% 
  st_drop_geometry() %>% merge(
            ., 
            stations_geo %>%  st_drop_geometry(),
            by="station_id",
            #all.y = TRUE,
            suffix = c("","_sp"))

# Aggregation stats ----
## Level 0 aggregation ----
stations_daily_ts_compiled_all_grouped <- stations_daily_summary_compiled_lite %>% 
  group_by(geography)
stations_daily_ts_compiled_all_aggstats <- fx_varSum(df = stations_daily_ts_compiled_all_grouped,
                                                               fields = selected_variables,
                                                               prefix = "By_geography__", suffix = i,
                                                               folder = stats_outputs_path)

## Level 1 aggregation ----
stations_daily_ts_compiled_y_grouped <- stations_daily_summary_compiled_lite %>% 
  group_by(YYYY)
stations_daily_ts_compiled_y_aggstats <- fx_varSum(df = stations_daily_ts_compiled_y_grouped,
                                                             fields = selected_variables,
                                                             prefix = "By_year__", suffix = i,
                                                             folder = stats_outputs_path)

stations_daily_ts_compiled_m_grouped <- stations_daily_summary_compiled_lite %>% 
  mutate(Month = lubridate::month(YYYY_MM_DD)) %>%
  group_by(Month)
stations_daily_ts_compiled_m_aggstats <- fx_varSum(df = stations_daily_ts_compiled_m_grouped,
                                                             fields = selected_variables,
                                                             prefix = "By_month__", suffix = i,
                                                             folder = stats_outputs_path)

stations_daily_ts_compiled_s_grouped <- stations_daily_summary_compiled_lite %>% 
  group_by(state)
stations_daily_ts_compiled_s_aggstats <- fx_varSum(df = stations_daily_ts_compiled_s_grouped,
                                                             fields = selected_variables,
                                                             prefix = "By_state__", suffix = i,
                                                             folder = stats_outputs_path)

stations_daily_ts_compiled_c_grouped <- stations_daily_summary_compiled_lite %>% 
  group_by(Climate_Region)
stations_daily_ts_compiled_c_aggstats <- fx_varSum(df = stations_daily_ts_compiled_c_grouped,
                                                             fields = selected_variables,
                                                             prefix = "By_climateregion__", suffix = i,
                                                             folder = stats_outputs_path)

fx_saveCSVPar(
  stations_daily_ts_compiled_all_aggstats,
  stations_daily_ts_compiled_y_aggstats,
  stations_daily_ts_compiled_m_aggstats,
  stations_daily_ts_compiled_s_aggstats,
  stations_daily_ts_compiled_c_aggstats,
  
  prefix = paste(threshold_label, "complete", sep = "_"),
  suffix = st_label,
  output_path = stats_outputs_path)

rm(stations_daily_summary_compiled_lite)

log_success("Aggregation stats completed")

}, error = function(e){
  log_warn("Aggregation stats failed")
  log_error(conditionMessage(e))
})

# Compute stations Compute baseline distribution  -----
tic()
stations_daily_means_rolling_avg <- stations_daily_summary_compiled %>% 
  mutate(DATE = YYYY_MM_DD) %>%
  mutate(dm_apparent_temperature = apparent_temperature) %>% # dm:daily mean
  arrange(DATE) %>%
  group_by(station_id) %>%
  # apparent temperature averaged over the immediate past three-day period
  mutate(dm_at_3_0 = round(runner::mean_run(x = apparent_temperature,
                                            k = 3L, lag = 0L, idx = DATE,
                                            na_rm=TRUE), 2)) %>%
  # apparent temperature averaged over 30 days prior lagged by three days
  mutate(dm_at_32_m3 = round(runner::mean_run(x = apparent_temperature,
                                              k = 32L, lag = 3L, idx = DATE,
                                              na_rm=TRUE), 2)) %>% ungroup() %>%
  # defined the climatalogical period of analysis including 35 days prior (to compute rolling means)
  filter(DATE < as.POSIXct("2023-01-01") &
         DATE > as.POSIXct("2007-11-25") ) %>%
  #mutate(test = n_distinct(YYYY)) %>%
  data.table()
#
dim(stations_daily_summary_compiled)
#hist(stations_daily_summary_compiled$YYYY)
#
dim(stations_daily_means_rolling_avg)
#hist(stations_daily_means_rolling_avg$YYYY)

# Distribution (percentiles) of Mean Daily Apparent Temperature  ----
dm_apparent_temperature_percentiles <- stations_daily_means_rolling_avg[ ,
                                                                     c(list(median = median(dm_apparent_temperature, na.rm = T)),
                                                                       as.list(round(quantile(dm_apparent_temperature, c(.01, .02, .05, .075, .10, .15, .25,
                                                                                                                         .85, .90, .925, .95, .98, .99),
                                                                                              na.rm = T),2))
                                                                     ),
                                                                     by = .(station_id)] %>%
  janitor::clean_names()

dm_apparent_temperature_compiled <- merge(stations_daily_means_rolling_avg,
                                          dm_apparent_temperature_percentiles,
                                          by.x=c("station_id"),
                                          by.y=c("station_id"),
                                          all.x = TRUE)

#glimpse(dm_apparent_temperature_compiled)
fx_toc(dm_apparent_temperature_compiled, 0, threshold_label, time_period)

#sum(is.na(Sheridan$station_name))

# Compute Sheridan terms ----
tic()
Sheridan <- dm_apparent_temperature_compiled %>%
  # drop those rows with missing apparent temperature (while still can have rolling average)
  #filter(!is.na(dm_apparent_temperature)) %>%

  # the acclimatization term
  mutate(EHaccl = dm_at_3_0 - dm_at_32_m3) %>%

  # excess heat based on the Nairn and Fawcett
  mutate(EH = pmax(0, (dm_at_3_0 - x95_percent))) %>%
  mutate(EHF = pmax(0, EH) * pmax(1, EHaccl)) %>%

  mutate(EC = pmin(0, (dm_at_3_0 - x5_percent))) %>%
  mutate(ECF = -1 * pmin(0, EC) * pmin(-1, EHaccl)) %>%

  merge(.,
        noaa_stations_sp, # %>% st_drop_geometry(),
        by.x="station_id",
        by.y="station_id",
        all.x = TRUE,
        suffix = c("","_sp")) %>%
  filter(!is.na(station_name))

fx_toc(Sheridan, 0, threshold_label, time_period)
#glimpse(Sheridan)

distribution_geo_set <- c(#"climate_region", 
                          "station_id", 
                          "county_name"
                          )#, "county_state_name")#, "geography")

#map(distribution_geo_set, function(distribution_set) {
for(distribution_set in distribution_geo_set) {
  #distribution_set <-  c("Climate_Region")
  tic()
  # code block
  # for(distribution_set in c( # * * * distribution_set ----
  #   # foreach(distribution_set = c(
  #     "station_id",
  #     "county_name",
  #     "state",
  #     "geography"
  #   #  ), .verbose = TRUE) %dopar% {
  # )){

  # Identify extreme events ----
  EHF_percentiles <- Sheridan[ EHF > 0,
                               c(list(median = median(EHF, na.rm = T)),
                                 as.list(round(quantile(EHF, c(.01, .02, .05, .10, .15, .25,
                                                               .75, .85, .90, .95, .98, .99), na.rm = T),2))
                               ),
                               by = distribution_set]  %>% clean_names() %>% #, state, station_id, county_name
    rename_with(.cols = where(is.numeric), function(x){paste0("ehf_", x)})


  ECF_percentiles <- Sheridan[ ECF < 0,
                               c(list(median = median(ECF, na.rm = T)),
                                 as.list(round(quantile(ECF, c(.01, .02, .05, .10, .15, .25,
                                                               .75, .85, .90, .95, .98, .99), na.rm = T),2))
                               ),
                               by = distribution_set]  %>% clean_names() %>% #, state, station_id, county_name
    rename_with(.cols = where(is.numeric), function(x){paste0("ecf_", x)})

  #ls(Sheridan)
  #glimpse(EHF_percentiles)
  #ls(ECF_percentiles)

  Sheridan_interim <- merge(Sheridan,
                            EHF_percentiles,
                            by.x= c(distribution_set),
                            by.y= c(distribution_set),
                            suffix  = c("", ""),
                            all.x = TRUE)
  dim(Sheridan_interim)
  #rm(Sheridan)

  Sheridan_compiled <- merge(Sheridan_interim,
                             ECF_percentiles,
                             by.x= c(distribution_set),
                             by.y= c(distribution_set),
                             suffix  = c("", ""),
                             all.x = TRUE)
  dim(Sheridan_interim)
  rm(Sheridan_interim)

  dim(EHF_percentiles)
  dim(ECF_percentiles)
  dim(Sheridan_compiled)

  #glimpse(Sheridan_compiled)
  # ***** Identify and tag individual days of extreme events ----
  stations_daily_means_compiled <- Sheridan_compiled %>%
    mutate(EHE = case_when(EHF > ehf_x85_percent ~ 1,
                           TRUE ~ 0)) %>%
    mutate(ECE = case_when(ECF < ecf_x15_percent ~ 1,
                           TRUE ~ 0))
  rm(Sheridan_compiled)
  # create spatial layer of identified extreme events----
  # stations_daily_means_compiled_sf <- st_as_sf(stations_daily_means_compiled,
  #                                              coords = c("lon_x", "lat_y"),
  #                                              crs = spatial_projection) %>% as_tibble() %>% st_as_sf()
  #
  # stations_daily_means_compiled_sp <- as_Spatial(stations_daily_means_compiled_sf,
  #                                                cast = TRUE)
  #
  # dim(stations_daily_means_compiled_sf)
  # plot(stations_daily_means_compiled_sf[, "EHF"])
  # plot(stations_daily_means_compiled_sp[, "EHF"])
  #
  # glimpse(stations_daily_means_compiled_sf)
  # glimpse(stations_daily_means_compiled_sp@data)
  #
  ## Label consecutive heat/cold days -----
  EHE <- stations_daily_means_compiled[, .(EHE = EHE[1],
                                           DATE = DATE,

                                           EH = EH,
                                           EHF = EHF,
                                           #EHMI = EHF * .N,
                                           EHMI = EHF * cumsum(rleidv(EHE)),
                                           DM_AT = dm_apparent_temperature,
                                           EHE_day = cumsum(rleidv(EHE)),
                                           EHE_duration = .N,
                                           EHE_start = first(DATE),
                                           EHE_end = last(DATE),
                                           #Longitude = lon_x,
                                           #Latitude = lat_y,
                                           Event_Type = "Extreme Heat Event"#,
  ),
  by = .(station_id, rleidv(EHE))][EHE == 1][, c('EHE', 'rleidv') := NULL][]%>%
    mutate(UID = paste0(strftime(EHE_start, format = "%Y%m%d"),
                        formatC(EHE_duration, width = 2, format = "d", flag = "0"),
                        "1")) %>%
    mutate(EUID = paste0(strftime(EHE_start, format = "%Y%m%d"),
                         strftime(EHE_end, format = "%Y%m%d"),
                        "1")) %>%
    mutate(STEUID = paste0(station_id, UID))


  ECE <- stations_daily_means_compiled[, .(ECE = ECE[1],
                                           DATE = DATE,
                                           
                                           EC = EC,
                                           ECF = ECF,
                                           #ECMI = ECF * .N,
                                           ECMI = ECF *cumsum(rleidv(ECE)),
                                           DM_AT= dm_apparent_temperature,
                                           ECE_day = cumsum(rleidv(ECE)),
                                           ECE_duration = .N,
                                           ECE_start = first(DATE),
                                           ECE_end = last(DATE),
                                           #Longitude = lon_x,
                                           #Latitude = lat_y,
                                           Event_Type = "Extreme Cold Event"#,
  ),
  by = .(station_id, rleidv(ECE))][ECE == 1][, c('ECE', 'rleidv') := NULL][] %>%
    mutate(UID = paste0(strftime(ECE_start, format = "%Y%m%d"),
                        formatC(ECE_duration, width = 2, format = "d", flag = "0"),
                        "2")) %>%
    mutate(EUID = paste0(strftime(ECE_start, format = "%Y%m%d"),
                         strftime(ECE_end, format = "%Y%m%d"),
                         "2")) %>%
    mutate(STEUID = paste0(station_id, UID))
  # EHE_ECE <- merge(EHE,
  #                             ECE,
  #                             by=c("station_id", "DATE", "Event_Type",
  #                                  "UID", "DM_apparent_temperature"),
  #                             #suffix = c("", ""),
  #                             all.x = TRUE, all.y = TRUE
  # )

  EHE_ECE <- dplyr::full_join(EHE,
                        ECE,
                        by=c("station_id", "DATE", "Event_Type",
                             "UID", "EUID", "STEUID", "DM_AT")
                        ) %>%
     mutate(Event_Type = replace_na(Event_Type, "Non-Extreme Weather")) %>%
     mutate_at(vars(EHE_duration, ECE_duration,
                    EH, EC,
                    EHF, ECF,
                    EHMI, ECMI), ~replace_na(., 0)) %>%
     mutate(Event_duration = EHE_duration + ECE_duration) %>%

     mutate(completeness_code = threshold_label) %>%
     mutate(cc_label = recode_factor(as.character(completeness_code), !!!code_labels)) %>%
     mutate(EF_distribution_set = distribution_set)
  
  fx_toc(EHE_ECE, 0, threshold_label, distribution_set)
  
  tic()
      # Compile stations daily values with identified events-----
  
  stations_daily_means_rolling_avg <- stations_daily_means_rolling_avg  %>%
    filter(YYYY >=  start_year &
           YYYY <=  end_year) %>% 
    dplyr::select(!contains("completeness_code") &
                  #!ends_with("lib") &
                  !contains("column") &
                  !contains("MM")) %>% 
    st_drop_geometry()

       EHE_ECE_compiled <- merge(
                                 EHE_ECE,
                                 stations_daily_means_rolling_avg,
                                 by.x=c("station_id", "DATE"),
                                 by.y=c("station_id", "DATE"),
                                 all.x = TRUE) %>%
         mutate_at(vars(EHE_duration, ECE_duration,
                        EH, EC,
                        EHF, ECF,
                        EHMI, ECMI), ~replace_na(., 0)) %>% 

       mutate(
        YYYY = lubridate::year(DATE),
        week_number = isoweek(DATE),
        day_number = yday(DATE),
        month_number = lubridate::month(format(DATE, "%Y-%m-%d")),
        month_name = lubridate::month(format(DATE, "%Y-%m-%d"), label = TRUE),
        season = fct_collapse(
          .f = month_name,
          Spring = c("Mar", "Apr", "May"),
          Summer = c("Jun", "Jul", "Aug"),
          Autumn = c("Sep", "Oct", "Nov"),
          Winter = c("Dec", "Jan", "Feb"))) %>%
      rename_at("YYYY", ~ "year_number") %>% 
      mutate(year_factor = as.factor(year_number)) %>%
      mutate(month_factor = as.factor(month_name)) %>%
      mutate(season_factor = as.factor(season))
  
  fx_toc(EHE_ECE_compiled, 0, threshold_label, distribution_set)
  
  tic()
    # Subset the data for the study period -----
    EHE_ECE_events_only <- EHE_ECE_compiled %>%
    mutate(YYYY = lubridate::year(DATE)) %>%
      filter(YYYY >=  start_year &
             YYYY <=  end_year)

fx_toc(EHE_ECE_events_only, 0, threshold_label, distribution_set)

  # ECE EHE Summaries -----

  # << Export summarized stations data ----
  fx_reportFieldsPar(
    stations_daily_means_rolling_avg,
    stations_daily_means_compiled,

    EHE_ECE,
    EHE_ECE_compiled,
    EHE_ECE_events_only,

    prefix = paste(threshold_label, "complete", distribution_set, sep = "_"),
    suffix = st_label,
    output_path = meta_output_path)

  fx_saveCSVPar(
    EHE_ECE_events_only,

    prefix = paste(threshold_label, "complete", distribution_set, sep = "_"),
    suffix = st_label,
    output_path = tables_output_path)

  fx_saveRObjects(
    EHE,
    ECE,
    EHE_ECE,

    prefix = paste(threshold_label, "complete", distribution_set, sep = "_"),
    suffix = st_label,
    output_path = params_output_path)

  fx_saveRObjects(
    EHE_ECE_compiled,
    EHE_ECE_events_only,

    prefix = st_label,
    suffix = paste(threshold_label, "complete", distribution_set, sep = "_"),
    output_path = rds_output_path)

  #gc()
  #
  #ls(EHE_ECE_compiled)

# * 3 ----
   source(here::here("ii_extreme_events_identification",
                      "0501_ece_ehe_summaries.R"), local=T)
  gc()
  }#) # end of the MAP loop ----

  #gc()
  print(Sys.time())
  } # end of the FOR loop ----
  #}) # end of the 2nd MAP loop ----
  #
print(showConnections())
print("Closing")
#print(stopCluster(cl))
#print(rm(cl))

cat("05 processes completed at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

cat("Total memory available: ", format(as.numeric(system("vmstat -s | grep 'total memory' | awk '{print $1}'", intern = TRUE)), big.mark = ","), "\n")
cat("Free memory available: ", format(as.numeric(system("vmstat -s | grep 'free memory' | awk '{print $1}'", intern = TRUE)), big.mark = ","), "\n")
cat("Number of physical cores: ", detectCores(logical = FALSE), "\n")
cat("Number of available threads: ", detectCores(logical = TRUE), "\n")
# Set the destination directory where you want to copy the files
#destination_dir <- file.path(source_dir, "05_noaa_isd_identified_ehe_ece")
##destination_dir <- file.path(source_dir, "EHCMI_byDay_new")

# Get the list of files matching the pattern in the source directory
##files_to_copy <- list.files(rds_output_path, pattern = "EHE_ECE_events_only", full.names = TRUE)

# Copy the files to the destination directory
##file.copy(files_to_copy, destination_dir, overwrite = TRUE)
