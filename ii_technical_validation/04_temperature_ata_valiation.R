## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

library(here)
library(furrr)
library(purrr)
library(tictoc)
library(getopt)
library(data.table)
library(tidyverse)
library(janitor)
library(sf)
library(doParallel)

# Set up computational parameters ----
spatial_projection  = 4269 #NAD83
#plots dimensions
ww = 22 #width
hh = 28 #height

spec = matrix(c(
  #'state', 's', 1, "character",
  #'year'  , 'y', 1, "character",
  'cores', 'c', 1, "character",
  'ratio', 'r', 1, "character",
  'job_id', 'j', 1, "character"
), byrow=TRUE, ncol=4)
opt = getopt(spec)

job_id <- opt$job_id

start_year <- opt$year
start_year <-  2008L  # // *0 [settings] O2 / Year ====
end_year <-  2022L

start_date <-  as.Date(paste0(start_year, "-01-01"), format="%Y-%m-%d")
end_date <-  as.Date(paste0(end_year, "-12-31"), format="%Y-%m-%d")
time_period <- paste(start_year, end_year, sep = "_")
geography <- paste("us", sep = "_")
# // *2 [settings] O2 / Title ====
project_title <- "04_noaa_isd_imputed_daily_temperature"
task_title <- "validation_62_10_9_190"

sampling_ratio <- as.integer(opt$ratio)
runtime_params <- paste0("(", sampling_ratio, "_", 
                         100 - sampling_ratio, ")")

cores <- as.integer(opt$cores)
#cores <- 3 # // [runtime settings] O2 / Cores ==== 

pointer <- "archiveX"

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

print(Sys.time())

# Computational script ----

j <- 65
fx_io()

  threshold_label <- as.character(j) #(100-((7-i)*5))
  print(threshold_label)

  ### I/O -----
  inputs_path <- file.path(source_dir, "03_imputed_daily_values_7classes_sorted_us_49")

  tic()
  gridmet_at_noaa_isd_stations <- readRDS(file.path(source_dir,
                                         "20_max_planck",
                                         "conus_gridmet_at_noaa_stations_2008_2022.rds")) %>% 
  mutate(temperature_avg = round((tmin + tmax) / 2, 1)) %>% 
  mutate(YYYY_MM_DD = as.Date(format(day), "%Y-%m-%d")) %>% 
  dplyr::select(any_of(c("station_id", "YYYY_MM_DD", "temperature_avg"))) %>% 
  #dplyr::filter(!is.na(temperature_avg)) %>% 
  #mutate_if(is.double, ~ round(., 1))# %>% 
  filter(YYYY_MM_DD >= start_date &
        YYYY_MM_DD <= end_date)
  
  #glimpse(gridmet_at_noaa_isd_stations)
  fx_toc(gridmet_at_noaa_isd_stations, 1, time_period, j)
  st_crs(gridmet_at_noaa_isd_stations)$epsg
  st_crs(gridmet_at_noaa_isd_stations)$input
    
  stations_geo_path <- file.path(source_dir,
                                 "00_noaa_stations_data_catalog_2007_2022",
                                 "us49_stations_2007_2022.rds")
  
  states_file_path <- file.path(source_dir,
                                "00_base_spatial_layers_grids_census_admin_boundaries", 
                                "us49_states_geo_tigris.rds")
  states_geo <- readRDS(states_file_path)[[1]]

  states_geo_climate <- states_geo %>% st_drop_geometry() %>% 
    dplyr::select(contains("STUSPS") | contains("Climate_Region"))
  
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

  #plot(states_geo["Climate_Region"])
  #glimpse(stations_geo)
  #glimpse(us_noaa_isd_stations)

 columns_to_keep <- c("Climate_Region", 
                      "temperature_avg", 
                      "YYYY", "YYYY_MM_DD", 
                      "station_id", "station_year")
 
 tic()
 stations_daily_ts_compiled <- readRDS(file.path(inputs_path,
                                                      "imputed_daily_data", 
                                                      paste0("us_49_stations_daily_summaries_imputed_geo_2007_2022_",
                                                            threshold_label,
                                                            "complete.rds")))[[1]] %>%  
 filter(YYYY_MM_DD >= start_date &
        YYYY_MM_DD <= end_date) #%>%  #// *3 [settings] O2 / Size ====
        #sample_frac(0.022) # //----
 
 #glimpse(stations_daily_ts_compiled)
 
 length(unique(stations_daily_ts_compiled$station_id)) # number of distinct stations
 length(unique(stations_daily_ts_compiled$YYYY_MM_DD)) / 15 # number of days
 
  E_DATES <- stations_daily_ts_compiled %>%  #Create the list of the days within the daily time series
    dplyr::select(c(YYYY_MM_DD, YYYY)) %>%
    unique(.) %>%
    arrange(YYYY_MM_DD)
  
 daily_data_segments <- as.list(E_DATES[[1]])
        
 stations_daily_ts_compiled_sf <- stations_daily_ts_compiled %>% 
 dplyr::select(any_of(columns_to_keep)) %>% 
 mutate(station_year = paste(station_id, YYYY, sep = "_")) %>% 
            merge(
            ., 
            stations_geo,
            by="station_id",
            #all.y = TRUE,
            suffix = c("","_sp"))
 fx_toc(stations_daily_ts_compiled_sf, 1, time_period, j)
 

 # class(stations_daily_ts_compiled)
 # class(stations_geo)
 # glimpse(stations_daily_ts_compiled_sf)
 ls(stations_daily_ts_compiled_sf)
 
  # find the total combination of time/geography/stations for the sampling
 stations_daily_ts_compiled_lite <- stations_daily_ts_compiled_sf %>% st_drop_geometry() %>%
   dplyr::select(any_of(columns_to_keep)) %>% 
   dplyr::filter(!is.na(temperature_avg)) %>% 
   distinct(YYYY, Climate_Region, station_id, 
            .keep_all = TRUE) 

 dim(stations_daily_ts_compiled_lite)
 
 fx_reportFieldsPar(
   stations_geo,
   stations_daily_ts_compiled,
   stations_daily_ts_compiled_sf,
   stations_daily_ts_compiled_lite,
   gridmet_at_noaa_isd_stations,
   
   prefix = time_period,
   suffix = paste0("_ccode_", threshold_label),
   output_path = geo_output_path)
 
  library(doParallel)
  # # create a cluster object
  cl <- makeCluster(cores, outfile = file.path(runtime_path, paste0("foreach_logs_",
                                                                     time_period,
                                                                     ".txt")))
  # register the cluster
  registerDoParallel(cl)
 
  # {{ + initiate the temporal loop + }} ----
  # // *4 [settings] O2 / Foreach ====
foreach(i = seq(1:cores),
        .multicombine = TRUE,
        .verbose = TRUE,
        .packages =c("here", "tictoc", "data.table",
                     "sf", "sp", "janitor", "tidyverse",
                     "purrr", "furrr", "logger")
        ) %dopar% {
    
    library(here)
    library(tictoc)
    library(data.table)
    library(tidyverse)
    library(sf)
    library(sp)
    library(logger)
    library(purrr)
    library(furrr)
    
    sf_use_s2(FALSE)
    
    source(here::here("runtime_setup",
                      "1_helper_functions.R"), local = T)
    source(here::here("runtime_setup",
                      "1_helper_functions_geo.R"), local = T)
    
  #for(i in 1:1){ # // *5 [settings] O2 / For loop ====
    
    # Logger set up ----
    logger_file <- file.path(runtime_path, paste0(time_period,
                                                      "_logger_outputs_for_sample_",
                                                      i,
                                                      ".log"))
    ## define the file logger with log rotation enabled
    log_appender(appender_file(logger_file, max_files = 1L,
                               max_lines = Inf, max_bytes = Inf))
    
    log_threshold(TRACE)
    log_threshold()

            dir.create(file.path(runtime_path, paste0("sample_", i)))
            sample_output_path <- file.path(runtime_path, paste0("sample_", i))
            
            dir.create(file.path(sample_output_path, paste0("stats_", i)))
            stats_output_path <- file.path(sample_output_path, paste0("stats_", i))
            
 # Generate a stratified test set
            
 tic()
 sampling_pool <- stations_daily_ts_compiled_lite %>%
   #dplyr::select(all_of(c("Climate_Region", "YYYY", "station_id"))) %>% ##
   #mutate(station_year = paste(station_id, YYYY, sep = "_"))##
   group_by(YYYY, Climate_Region) %>% 
              select(-c("YYYY_MM_DD", "temperature_avg")) 
 fx_toc(sampling_pool, 1, time_period, i)
 
 write.csv(sampling_pool,
           paste0(tables_output_path, "/",
                  time_period, 
                  "_sampling_pool_[",
                  nrow(sampling_pool), "_records]",
                  "_(sample_", i,
                  ").csv"),
           row.names = TRUE)
 
 tic()
 sampled_stations <- sampling_pool %>%  # Group by year and state
   sample_frac(sampling_ratio / 100) # Sampling ----
 fx_toc(sampled_stations, 1, time_period, i)
   
   #glimpse(sampled_stations)
   #length(unique(sampled_stations$station_year))
   
  write.csv(sampled_stations,
              paste0(tables_output_path, "/",
                     time_period, 
                     "_sampled_stations_x_year_[",
                     nrow(sampled_stations), "_records]",
                     "_(sample_", i,
                     ").csv"),
              row.names = TRUE)

  # Split training and test set ----
  tic()
  stations_daily_ts_compiled_test20 <- stations_daily_ts_compiled_sf %>% 
                                            filter(., station_year %in% sampled_stations$station_year) %>% 
                                            st_as_sf()
  fx_toc(stations_daily_ts_compiled_test20, 1, time_period, i)
  
  tic()
  stations_daily_ts_compiled_train80 <- stations_daily_ts_compiled_sf %>% 
                                            filter(., !station_year %in% sampled_stations$station_year) %>% 
                                            st_as_sf()
  fx_toc(stations_daily_ts_compiled_train80, 1, time_period, i)
  
  rm(stations_daily_ts_compiled_sf)
  dim(stations_daily_ts_compiled_test20) # Number of the daily records based on the samples stations over time
  dim(stations_daily_ts_compiled_train80) 
  
  fx_saveRObjects(
    sampled_stations,
    stations_daily_ts_compiled_test20,
    stations_daily_ts_compiled_train80,
    prefix = "raw",
    
    suffix = paste0(time_period, "_sample_", i),
    output_path = sample_output_path)
  
  length(daily_data_segments) /15
  gc()

    tic()
    log_debug(paste0("(Sample ", i, ") Computing daily idw estimation started"))
  # * Estimate daily deltas -----
    result_list <- map(daily_data_segments, ~ {
      selected_day <- .
    
    selected_day_label <- as.character.Date(selected_day)
    #core_id <- (i %% cores)
    print(selected_day)

    ## Subset daily records for training and test sets ----
    stations_daily_ts_compiled_test20_tmp <- stations_daily_ts_compiled_test20 %>% 
      dplyr::select(all_of(columns_to_keep)) %>% 
      dplyr::filter(YYYY_MM_DD == selected_day) %>% 
      dplyr::filter(!is.na(temperature_avg)) %>% 
      as_Spatial(., cast = TRUE)

    stations_daily_ts_compiled_train80_tmp <- stations_daily_ts_compiled_train80 %>% 
      dplyr::select(all_of(columns_to_keep)) %>% 
      dplyr::filter(YYYY_MM_DD == selected_day) %>% 
      dplyr::filter(!is.na(temperature_avg)) %>% 
      as_Spatial(., cast = TRUE)
        
      #length(unique(stations_daily_ts_compiled_train80_tmp$station_id))
      #length(unique(stations_daily_ts_compiled_test20_tmp$station_id))
      
      ### IDW ----
      interpolated_idw_SP <- gstat::idw(as.formula("temperature_avg ~ 1"),
                                      stations_daily_ts_compiled_train80_tmp,
                                      newdata = stations_daily_ts_compiled_test20_tmp, 
                                      nmax = 6,
                                      idp = 2)
    interpolated_idw_SP <- interpolated_idw_SP[1]
    names(interpolated_idw_SP)[1] <- "temperature_avg_62_idw"
    
    ### IDW NN ----
    interpolated_idw_NN_SP <- gstat::idw(as.formula("temperature_avg ~ 1"),
                                      stations_daily_ts_compiled_train80_tmp,
                                      newdata = stations_daily_ts_compiled_test20_tmp, 
                                      nmax = 1,
                                      idp = 0)
    
    interpolated_idw_NN_SP <- interpolated_idw_NN_SP[1]
    names(interpolated_idw_NN_SP)[1] <- "temperature_avg_nn_idw"
    
    ### Gridmet ----
    gridmet_at_noaa_isd_stations_tmp <- gridmet_at_noaa_isd_stations %>%
      dplyr::filter(YYYY_MM_DD == selected_day) %>% 
      dplyr::filter(!is.na(temperature_avg))
    
    ## Combine all the daily estimates ----
    stations_daily_delta_computed <- cbind(stations_daily_ts_compiled_test20_tmp, 
                                                             interpolated_idw_SP[1],
                                                             interpolated_idw_NN_SP[1]) %>% st_as_sf() %>% 
      filter(!is.na(station_id)) %>% 
      mutate_if(is.double, ~ round(., 1)) %>% 
      merge(.,
            gridmet_at_noaa_isd_stations_tmp,
            by.x=c("station_id", "YYYY_MM_DD"),
            by.y=c("station_id", "YYYY_MM_DD"),
            all.x = TRUE,
            suffix = c("","_gridmet")) %>% 
      mutate(delta_idw = temperature_avg - temperature_avg_62_idw,
             delta_nn = temperature_avg - temperature_avg_nn_idw,
             delta_gridmet= temperature_avg - temperature_avg_gridmet) 
    #
    #glimpse(stations_daily_delta_computed)
    
      return(stations_daily_delta_computed)
    }) 
    log_trace(paste0("(Sample ", i, ") Computing daily deltas completed"))
    # Compile the computed test dataframe -----
    validation_compiled_test20_delta <- bind_rows(result_list) %>% st_as_sf() %>% 
      mutate(subset = "testing_set") %>% 
      mutate(sample = i) %>% 
      merge(.,
            stations_geo %>% st_drop_geometry(),
            by.x="station_id",
            by.y="station_id",
            all.x = TRUE,
            suffix = c("","_sp"))
    
    fx_toc(validation_compiled_test20_delta, 1, time_period, i)
    #glimpse(validation_compiled_test20_delta)
    
    tic()
    validation_compiled_train80_delta <- stations_daily_ts_compiled_train80 %>% 
    merge(.,
          gridmet_at_noaa_isd_stations,
          by.x=c("station_id", "YYYY_MM_DD"),
          by.y=c("station_id", "YYYY_MM_DD"),
          all.x = TRUE,
          suffix = c("","_gridmet")) %>% 
          mutate(delta_gridmet= temperature_avg - temperature_avg_gridmet,
             subset = "training_set",
             sample = i)
    fx_toc(validation_compiled_train80_delta, 1, time_period, i)
    
    rm(gridmet_at_noaa_isd_stations)
    rm(stations_daily_ts_compiled_train80)
    rm(stations_daily_ts_compiled_test20)
    
    fx_saveRObjects(
      validation_compiled_test20_delta,
      validation_compiled_train80_delta,
      prefix = time_period,
      suffix = paste0("_sample_", i),
      output_path = sample_output_path)
    
    fx_reportFieldsPar(
      validation_compiled_test20_delta,
      validation_compiled_train80_delta,

      prefix = paste0(time_period, "_sample_", i),
      suffix = "geo",
      output_path = geo_output_path)
    
    rm(validation_compiled_train80_delta)
    
    source(here::here("i_spatial_data_preprocessing",
                      "0401_samples_error_visualization.R"), local=T)
  }
  
  print(showConnections())
  print("Closing")
  print(stopCluster(cl))
  gc()
  