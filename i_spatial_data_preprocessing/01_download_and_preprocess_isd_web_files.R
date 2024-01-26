## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Retrieve NOAA ISD climate data
library(here)
library(data.table)
library(tidyverse)
library(sf)
library(logger)

sf_use_s2(F)

job_id <- ""
project_title <- "01_web_raw_hourly_data"

geography <- paste("us", 49L,
                   sep = "_")

model_params <- paste(geography,
                      "",
                      sep = "_")

task_title <- model_params

### Temp set ----
start_year <- 2007L
end_year <-  2022L

pointer <- "archivex"

time_period <- paste(start_year, end_year, sep = "_")

spatial_projection <-  3857 # #epsg 3857 that is commonly used for web mapping
spatial_projection_lonlat <- 4269 # CRS that is used to transform lon-lat data (based on NAD 83)

default_crs = sf::st_crs(spatial_projection_lonlat)
sf_use_s2(FALSE)

# I/O Settings -----
source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

dir.create(file.path(reports_path, paste0("raw_tables_", time_period)))
scratch_dir <- file.path(reports_path, paste0("raw_tables_", time_period))

dir.create(file.path(reports_path, paste0("processed_tables_", time_period)))
output_path <- file.path(reports_path, paste0("processed_tables_", time_period))

# read in station data for US from a CSV file in the temporary working directory and filter data
us_noaa_isd_stations <- readRDS(file.path(source_dir,
                                        "00_noaa_stations_data_catalog_2007_2022",
                                        "us49_stations_2007_2022.rds")) %>%
                                        filter(between(YYYY, start_year, end_year))

unique(us_noaa_isd_stations$YYYY)

length(unique(us_noaa_isd_stations$station_id))

### * 1 ----
station_list <- unique(us_noaa_isd_stations$station_id)#[1408:1412] #sample
length(station_list)


#### Selected_variables -----
selected_variables <- c(
  "temperature",
  "temperature_dewpoint",
  "wind_speed")

# retrieve stations raw data through a parallel loop----
# {{ + initiate the temporal loop + }} ----
library(doParallel)
### Cores ----
cores <- 16

# # create a cluster object
cl <- makeCluster(cores, outfile = file.path(runtime_path, paste0("foreach_logs_",
                                                                  time_period,
                                                                  timestamp,
                                                                  ".txt")))
# register the cluster
registerDoParallel(cl)

foreach(
  year_i = seq(start_year, end_year),
  .multicombine = TRUE,
  .verbose = TRUE,
  .packages =c("here", "data.table", "dplyr", "purrr",
               "tictoc", "logger")
) %dopar% {
  print(Sys.time())

  library(here)
  library(dplyr)
  library(purrr)
  library(data.table)
  library(logger)


  #Define variables for missing stations list
  erroneous_stations_list <- "Station_id"
  erroneous_stations_year <- "Year"
  erroneous_stations_message <- "Error_message"

  #Define variables for missing stations list
  compiled_stations_list <- "Station_id"
  parsing_type_list <- "Parsing_type"
  num_rows_list <- "Num_rows"
  num_cols_list <- "Num_columns"
  compiled_stations_year <- "Year"


  error_counter_i <- 0

  dir.create(file.path(scratch_dir, year_i))
  download_dest_path <- file.path(scratch_dir, year_i)

  dir.create(file.path(output_path, paste0(year_i, "_processed_rds_files")))
  yearly_path_rds <- file.path(output_path, paste0(year_i, "_processed_rds_files"))

  dir.create(file.path(output_path, paste0(year_i, "_processed_csv_files")))
  yearly_path_csv <- file.path(output_path, paste0(year_i, "_processed_csv_files"))

  # loop through the stations list #[1408:1412]
  
  download_hourly_tables <-  function(i) {
  ## create a folder storing the log files
  logger_file <- file.path(meta_output_path, paste0("logger_outputs_",
                                                    year_i,
                                                    ".log"))
  ## define the file logger with log rotation enabled
  log_appender(appender_file(logger_file, max_files = 1L,
                             max_lines = Inf, max_bytes = Inf))
  
  log_threshold(TRACE)
  log_threshold()
    
      each_station <- station_list[[i]]
      
    tic()
    #each_station <- "72472099999"
    # generate download urls based on the NOAA web api
    station_year_url <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/",
                               year_i, "/",
                               each_station, ".csv"
                               )
    # expect some download links cause errors
     tryCatch({
       # download by specific url
         each_station_filename <- paste0(each_station, ".csv")
         dumped_csv_path <- here(download_dest_path, each_station_filename)
         
         #Sys.sleep(5) # pause for a few seconds
         
         download.file(url = station_year_url,
                       destfile = dumped_csv_path,
                       quiet = TRUE,
                       mode = "wb")

         downloaded_csv_df <- fread(dumped_csv_path,

                                         header = TRUE,
                                         check.names = FALSE) %>%
           # clean up the columns with "," issue
           #select(-one_of('EQD', 'REM')) %>%
           mutate(REM = "REM", EQD = "EQD")

         # write the refined station tables to disk for further parsing
         write.csv(downloaded_csv_df,
                   file.path(yearly_path_csv,
                             paste0(each_station, "_",
                                    year_i,
                                    ".csv")),
                   row.names = FALSE)
         
          saveRDS(downloaded_csv_df,
                  file.path(yearly_path_rds,
                            paste0(each_station, "_",
                                   year_i,
                                   ".rds")))
         
         fx_toc(downloaded_csv_df, 0, each_station, year_i)
         
         cat("\n Station :", each_station, " table completed \n")

         # compile a data structure statistics for each station
         num_cols <- ncol(downloaded_csv_df)
         num_rows <- nrow(downloaded_csv_df)
         rm(downloaded_csv_df)

         return(list(each_station, num_cols, num_rows))
         
         }, error = function(e) {

           log_debug(each_station)
           log_warn("download error")
           log_error(conditionMessage(e))
           
           downloaded_csv_df <- NULL
           num_cols <- 0
           num_rows <- 0
         })
    
      gc()
      
       }

    
    runtime_collector <- map(1:length(station_list),  
                                    download_hourly_tables)
    
    station_id <- map(runtime_collector, `[[`, 1)
    num_cols <- map(runtime_collector, `[[`, 2)
    num_rows <- map(runtime_collector, `[[`, 3)
    
    nrow_raw_hourly_records_station_report <- data.frame(station_id = unique(unlist(station_id)), 
                                                         num_columns = unlist(num_cols),
                                                         raw_hourly_records = unlist(num_rows)) %>% 
      mutate(year = year_i)
    
    nrow_raw_yearly_records_station_report <- nrow_raw_hourly_records_station_report %>%
      group_by(year) %>%
      dplyr::summarise(
        total_stations_parsed = n(),
        across(raw_hourly_records,
               list(
                 sum = ~round(sum(as.integer(.), na.rm = T), 1),
                 avg = ~round(mean(as.integer(.), na.rm = T), 1),
                 min = ~round(min(as.integer(.), na.rm = T), 1),
                 max = ~round(max(as.integer(.), na.rm = T), 1)
               )
        ), .groups = 'drop')
    
    fx_appendCSV(nrow_raw_yearly_records_station_report,
                 prefix = "all",
                 suffix = "",
                 
                 output_path = reports_path)
    
  write.csv(nrow_raw_hourly_records_station_report,
            file.path(tables_output_path,
                      paste0(year_i, 
                             "_dowloaded_stations_report_",
                             sum(nrow_raw_hourly_records_station_report$raw_hourly_records),
                             ".csv")),
            row.names = FALSE)

  
} # closing the yearly loop
print(showConnections())

print("Closing")
print(stopCluster(cl))


log_success("End of script")
