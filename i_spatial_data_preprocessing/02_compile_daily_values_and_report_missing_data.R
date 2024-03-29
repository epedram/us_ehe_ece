## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Compile daily weather records based on the hourly NOAA ISD climate data
## Visualize missing daily records

library(here)
library(data.table)
library(tidyverse)
library(sf)
library(getopt)
library(tictoc)
library(furrr)

spec = matrix(c(
  #'state', 's', 1, "character",
  'year'  , 'y', 1, "character",
  'cores'  , 'c', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)

opt = getopt(spec)
job_id <- opt$job_id

### Cores ----
cores <- opt$cores  

pointer <- "archiveX"

project_title <- "02_compiled_tables_of_daily_values_15years"

# set up the parameters ----
geography <- paste("us", 49L,
                   sep = "_")

### Temp set 0 ----
start_year <- opt$year

end_year <-  start_year

time_period <- paste(start_year, end_year, sep = "_")

model_params <- paste(time_period,
                      sep = "_")

task_title <- geography

spatial_projection <-  3857 # #epsg 3857 that is commonly used for web mapping
spatial_projection_lonlat <- 4269 # CRS that is used to transform lon-lat data (based on NAD 83)

default_crs = sf::st_crs(spatial_projection_lonlat)
sf_use_s2(FALSE)

# I/O Settings -----
source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

# read in station data from a CSV file in the temporary working directory 
input_dir <- file.path(source_dir,
                       "01_web_raw_hourly_data_2007_2022",
                       "processed_tables_2007_2022")


dir.create(file.path(reports_path, paste0("missing_data_visualization")))
missing_data_output_path <- file.path(reports_path, paste0("missing_data_visualization"))


# set up computational parameters ----
#### Selected_variables -----
selected_variables <- c(
  "temperature",
  "temperature_dewpoint",
  "wind_speed")

#### Eliminated_types -----
eliminated_types <- c(
  "", " ",
  "SOM", "SOD", 
  "COOPD", "COOPS",
  "PCP15", "PCP60")

# {{ + initiate the temporal loop + }} ----
year_i = start_year
             

print(Sys.time())

    library(here)
    library(tidyverse)
    library(data.table)
    library(isdparser)
    library(naniar)
    library(visdat)
    library(imputeTS)
    library(tictoc)
    library(logger)

    # Read the raw data in CSV format from permanent storage
    stations_data_path <- file.path(input_dir,
                                    paste0(year_i, "_processed_csv_files"))

    annual_csv_list <- list.files(path = stations_data_path,
                                  pattern = ".csv") %>% #[1228:1253]
      str_sub(., start = 1L, end = 11L) # // ===== [1:180] ====
    
    write.csv(annual_csv_list,
              file.path(reports_path,
                        paste0(year_i,
                               "_lisf_of_downloaded_csv_yearly_tables_n", 
                               length(annual_csv_list),
                               ".csv")),
              row.names = T)

    parsing_type_list <- list()

    hourly_to_daily <-  function(i) {
      gc()
      tic()
      rows_counter <- 0
      
      ## create a folder storing the log files
      logger_file <- file.path(meta_output_path, paste0("logger_outputs_",
                                                    year_i,
                                                    ".log"))
      ## define the file logger with log rotation enabled
      log_appender(appender_file(logger_file, max_files = 1L,
                                 max_lines = Inf, max_bytes = Inf))
      
      log_threshold(TRACE)
      log_threshold()
      
      cat(year_i, i, " of ", length(annual_csv_list))

          
        each_station_id <- annual_csv_list[[i]] 

        each_station_cvs_path <- file.path(stations_data_path,
                                           paste0(each_station_id, 
                                                  "_",
                                                  year_i,
                                                  ".csv"))
        tryCatch({
          parsed_web_file <- isdparser::isd_parse_csv(each_station_cvs_path) %>%
          ## added to resolve the transformation function bug
          mutate(total_chars = 0L)
          
          parsing_type <- "by_table"
          #log_success(each_station_id)
            
          
        each_station_hourly_nrow <- nrow(parsed_web_file)
        #rows_counter <- rows_counter + each_station_hourly_nrow

        ## handle data type and unit conversions
        ### filter out certain report types ----
        transformed_parsed_web_file <- isdparser::isd_transform(parsed_web_file) %>%
          mutate(station_id = as.character(station)) %>%
          as_tibble() %>%
          dplyr::filter(!grepl(' ', report_type)) %>%
          dplyr::filter(!report_type %in% eliminated_types)
        
        rm(parsed_web_file)
        setDT(transformed_parsed_web_file)
        
        if (!all(selected_variables %in% colnames(transformed_parsed_web_file))) {
          missing_columns <- setdiff(selected_variables, colnames(transformed_parsed_web_file))
          log_debug(each_station_id)
          log_warn(missing_columns)
          stop(paste("Error: Missing columns in dataset", i, ":", paste(missing_columns, collapse = ", ")))
        }

        ### populate placeholder for missing dates
        each_station_hourly_df <- transformed_parsed_web_file %>%
          mutate(
            YYYY_MM_DD = as.Date(format(date), "%Y-%m-%d"),
            month_name = lubridate::month(format(date, "%Y-%m-%d"), label = TRUE)) %>%
          mutate(YYYY = year_i) %>%
          naniar::replace_with_na_at(.vars = selected_variables,
                                     condition = ~.x > 900) # to remove invalid temperature readings

        fx_toc(each_station_hourly_df, 1, each_station_id, year_i)
        rm(transformed_parsed_web_file)
        
        print("Computing summary stats for individual station")
        print("Daily")

        ## compute daily summaries for one station for a certain year----
        station_daily_compiled <- each_station_hourly_df %>%
          group_by(station_id,
                   YYYY_MM_DD) %>%
          dplyr::summarise(
            num_columns_day = ncol(.),
            num_rec_per_day_n = n(),
            report_types_day = toString(unique(report_type)),

            across(all_of(selected_variables),
                   list(
                     avg = ~round(mean(as.integer(.), na.rm = T), 1),
                     min = ~round(min(as.integer(.), na.rm = T), 1),
                     max = ~round(max(as.integer(.), na.rm = T), 1)
                   )
            ), .groups = 'drop') %>%
            tidyr::complete(station_id, num_columns_day,
                            YYYY_MM_DD = seq.Date(as.Date(paste0(year_i, "-01-01"), "%Y-%m-%d"),
                                                  as.Date(paste0(year_i, "-12-31"), "%Y-%m-%d"),
                                                  by="day")) %>%
          mutate(YYYY = year_i) %>%
          # convert inf to nulls
          mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
          mutate(
            month_name = lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE)
            ) %>% 
          mutate(num_rec_per_day_n = replace_na(num_rec_per_day_n, 0))

        rm(each_station_hourly_df)
        

        print("Annual")
        ## compute annual summaries for one station for a certain year----
        station_annual_summary <- station_daily_compiled %>%
          group_by(station_id) %>%
          dplyr::summarise(
            num_columns = max(num_columns_day, na.rm=T),
            num_rec_per_year_sum_n = sum(num_rec_per_day_n, na.rm=T),
            num_distinct_days = n_distinct(YYYY_MM_DD, na.rm=T),
            distinct_days_ratio = round(n_distinct(YYYY_MM_DD, na.rm=T)/365, 2) ,

            temperature_comp_ratio = round(sum(!is.na(temperature_avg))/365, 3),
            temperature_dewpoint_comp_ratio = round(sum(!is.na(temperature_dewpoint_avg))/365, 3),
            wind_speed_comp_ratio = round(sum(!is.na(wind_speed_avg))/365, 3),

            .groups = 'drop') %>%
          mutate(YYYY = year_i) %>%
          mutate(num_obs_per_year = each_station_hourly_nrow,
                 avg_num_obs_per_day = round(num_obs_per_year/num_distinct_days, 1)) %>% 
          # convert inf to nulls
          mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

        cat("\n", annual_csv_list[[i]], "completed")

        # # # visualize missing daily values + + + ----
        
        daily_selected_vars <- station_daily_compiled %>%
          dplyr::select(ends_with("station_id") | starts_with("YYYY_MM_DD") |
                          ends_with("_avg") | ends_with("_min") | ends_with("_max")) %>%
          tidyr::complete(station_id,
                          YYYY_MM_DD = seq.Date(as.Date(paste0(year_i, "-01-01"), "%Y-%m-%d"),
                                   as.Date(paste0(year_i, "-12-31"), "%Y-%m-%d"),
                                   by="day")
          )

        return(list(station_daily_compiled, station_annual_summary, 
                    each_station_hourly_nrow, each_station_id))
        
      }, error = function(e){

        parsed_web_file <- isdparser::isd_parse(each_station_cvs_path, additional = FALSE)
        parsing_type <- "by_row"

        log_debug(each_station_id)
        log_warn("parsed by row / unit conversion error")
        log_error(conditionMessage(e))
        
        cat(each_station_id, "ERROR :",conditionMessage(e), "\n")
      })

    }    # # closing the internal loop ----
      
      # Set up future map vector
      plan(multisession, workers = cores)
      
      runtime_collector <- future_map(1:length(annual_csv_list), 
                                      hourly_to_daily)
      
      sum_nrow <- map(runtime_collector, `[[`, 3)
      station_id_nrow <- map(runtime_collector, `[[`, 4)
      
      nrow_raw_hourly_records_station_report <- data.frame(station_id = unlist(station_id_nrow), 
                                                           raw_hourly_records = unlist(sum_nrow)) %>% 
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
      
      fx_saveCSVPar(nrow_raw_hourly_records_station_report,
                     prefix = year_i,
                     suffix = sum(nrow_raw_hourly_records_station_report$raw_hourly_records, na.rm = T),
       
                     output_path = tables_output_path)
      
    # ## create a folder storing the log files
     logger_file <- file.path(reports_path, paste0("logger_outputs_collector",
                                                       ".log"))
    # ## define the file logger with log rotation enabled
     log_appender(appender_file(logger_file, max_files = 1L,
                                max_lines = Inf, max_bytes = Inf))

     dir.create(file.path(reports_path, paste0("compiled_annual_data")))
     annual_output_path <- file.path(reports_path, paste0("compiled_annual_data"))
     
     length(runtime_collector)
     runtime_collector[9]
      
     tryCatch({
      stations_annual_summary <- map(runtime_collector, `[[`, 2)
      stations_annual_summary <- bind_rows(stations_annual_summary) %>%
        mutate(year_factor = as.factor(YYYY))

      setDT(stations_annual_summary)
      log_info("Compiling cumulative annual tables completed")
      
      saveRDS(stations_annual_summary, file=file.path(annual_output_path,
                                                    paste0("stations_annual_compiled_tables_",
                                                    year_i,
                                                    ".rds")))
      
      write.csv(stations_annual_summary,
                file.path(annual_output_path,
                          paste0("stations_annual_compiled_tables_",
                                 year_i,
                                 ".csv")),
                row.names = T)

    # bind daily climate data for all the stations for a certain year----
      dir.create(file.path(reports_path, paste0("compiled_daily_data")))
      daily_output_path <- file.path(reports_path, paste0("compiled_daily_data"))
      
    print("Start compiling cumulative yearly tables")
     stations_daily_summary <- map(runtime_collector, `[[`, 1)
     stations_daily_summary <- bind_rows(stations_daily_summary) %>%
      mutate(
        month_name = as.factor(lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE))
        ) %>%
      mutate(year_factor = as.factor(YYYY)) %>%
      mutate(month_factor = as.factor(month_name)) 
     
      setDT(stations_daily_summary)
      log_info("Compiling cumulative daily tables completed")

      ## < Export summarized stations data ----
      saveRDS(stations_daily_summary, file=file.path(daily_output_path,
                                                   paste0("stations_daily_compiled_tables_",
                                                   year_i,
                                                   ".rds")))
      
      write.csv(stations_daily_summary,
                file.path(daily_output_path,
                          paste0("stations_daily_compiled_tables_",
                                 year_i,
                                 ".csv")),
                row.names = T)
      
    fx_reportFieldsPar(
      stations_daily_summary,
      stations_annual_summary,
      
      prefix = year_i,
      suffix = "missing_per_year",
      output_path = meta_output_path)
    
     }, error = function(e){
       log_debug(each_station_id)
       log_warn("compiling error")
       log_error(conditionMessage(e))
      
     })

    log_info("End of script")
    