## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Impute missing daily records
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
  #'year'  , 'y', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)

opt = getopt(spec)
job_id <- opt$job_id
cores <- 3L

pointer <- "archiveX"

project_title <- "03_imputed_daily_values_7classes_sorted_by_station_data_viz"
runtime_params <- "data_viz"
# set up the parameters ----
geography <- paste("us", 49L,
                   sep = "_")
geog <- c("Massachusetts", "MA")

### Temp set 0 ----
start_year <- 2007L # // [runtime settings] O2 ====
end_year <-  2022L
maxcode <- 7

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
                  "2_plotting_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

# read in station data from a CSV file in the temporary working directory 
input_dir <- file.path(source_dir,
                       "02_compiled_tables_of_daily_values_us_49")

file.exists(source_dir)
file.exists(input_dir)

# set the I/O path -----

dir.create(file.path(reports_path, paste0("imputed_daily_data")))
imputed_outputs_path <- file.path(reports_path, paste0("imputed_daily_data"))

dir.create(file.path(reports_path, paste0("variables_distribution")))
distplot_outputs_path <- file.path(reports_path, paste0("variables_distribution"))


# Load daily and annual summaries from compiled yearly files
processed_daily_data_path <- file.path(input_dir,
                                       "compiled_daily_data")
file.exists(processed_daily_data_path)
list.files(processed_daily_data_path)

processed_annual_data_path <- file.path(input_dir,
                                        "compiled_annual_data")
list.files(processed_annual_data_path)

daily_processed_data_list <- list.files(path = processed_daily_data_path,
                                       pattern = "*rds*")

annual_processed_data_list <- list.files(path = processed_annual_data_path,
                                        pattern = "*rds*")


# Merge all the yearly tables for the entire study period-----
fx_completeness_code_to_daily <-  function(i) {
  gc()
  print(i)

  # ANNUAL ----

  each_year_annual_df <- readRDS(file.path(processed_annual_data_path,
                                         annual_processed_data_list[[i]])) %>%

    mutate(completeness_above_95_percent =
             case_when(
               (temperature_comp_ratio >= 0.95 &
                 temperature_dewpoint_comp_ratio >= 0.95 &
                 wind_speed_comp_ratio >= 0.95)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_90_percent =
             case_when(
               (temperature_comp_ratio >= 0.90 &
                 temperature_dewpoint_comp_ratio >= 0.90 &
                 wind_speed_comp_ratio >= 0.90)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_85_percent =
             case_when(
               (temperature_comp_ratio >= 0.85 &
                 temperature_dewpoint_comp_ratio >= 0.85 &
                 wind_speed_comp_ratio >= 0.85)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_80_percent =
             case_when(
               (temperature_comp_ratio >= 0.80 &
                 temperature_dewpoint_comp_ratio >= 0.80 &
                 wind_speed_comp_ratio >= 0.80)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_75_percent =
             case_when(
               (temperature_comp_ratio >= 0.75 &
                 temperature_dewpoint_comp_ratio >= 0.75 &
                 wind_speed_comp_ratio >= 0.75)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_70_percent =
             case_when(
               (temperature_comp_ratio >= 0.70 &
                  temperature_dewpoint_comp_ratio >= 0.70 &
                  wind_speed_comp_ratio >= 0.70)
               ~ 1L, TRUE ~ 0)) %>%

    mutate(completeness_code = 
             # 6 classes with minimum threshold and 1 class for the rest (7 in total)
             completeness_above_70_percent + completeness_above_75_percent +
             completeness_above_80_percent + completeness_above_85_percent +
             completeness_above_90_percent + completeness_above_95_percent,

             completeness_percent = 100-((maxcode - completeness_code)*5))
  
  glimpse(each_year_annual_df)

  # DAILY ----
  each_year_annual_df_qc <- each_year_annual_df %>%
    dplyr::select(ends_with("_id") | ends_with("completeness_code") |
                    contains("YYYY") )

  each_year_daily_df_raw <- readRDS(file.path(processed_daily_data_path,
                                        daily_processed_data_list[[i]])) %>%
    # adjust the columns data type
    mutate(station_id = as.character(station_id)) %>%
    mutate(YYYY_MM_DD = as.Date(format(YYYY_MM_DD), "%Y-%m-%d"))
  
  # assign completeness info to daily tables
  each_year_daily_df <-
    merge(each_year_daily_df_raw,
          each_year_annual_df_qc,
          #
          by.x = c("station_id", "YYYY"),
          by.y = c("station_id", "YYYY"),
          all.x = TRUE)
  
  return(list(each_year_daily_df, each_year_annual_df))

}
# Compile summaries ----
## Annual-----
#### Assign "Completeness" flags ----
  # Set up future map vector
  plan(multisession, workers = cores)
  
  runtime_collector <- future_map(1:length(annual_processed_data_list), 
                                  fx_completeness_code_to_daily)
  
  all_annual_summaries <- map(runtime_collector, `[[`, 2)
  all_annual_summaries <- bind_rows(all_annual_summaries) %>%
                                    dplyr::filter(YYYY >= start_year)
  
glimpse(all_annual_summaries)
dim(all_annual_summaries) / 16

## Daily-----
## Replace inf values with NA
tic()
all_daily_summaries_inf2na <- map(runtime_collector, `[[`, 1)
all_daily_summaries_inf2na <- bind_rows(all_daily_summaries_inf2na) %>%
  dplyr::filter(YYYY >= start_year) %>%
  # convert inf to nulls
  mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))
fx_toc(all_daily_summaries_inf2na, 0, time_period, paste0(maxcode, "_classes_df"))

tic()
setDT(all_daily_summaries_inf2na)
fx_toc(all_daily_summaries_inf2na, 1, time_period, paste0(maxcode, "_classes_dt"))

selected_variables <- c(
  "temperature_avg",
  "wind_speed_avg",
  "temperature_dewpoint_avg",
  "YYYY_MM_DD"
)

hist(all_daily_summaries_inf2na$completeness_code)

# only summarize stations within the completeness class boundary but not cumulative 
all_daily_inf2na_notimputed <- all_daily_summaries_inf2na %>%
  mutate(completeness_percent = 100-((maxcode - completeness_code)*5)) %>%
  group_by(YYYY, completeness_percent) %>%
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
  .groups = 'drop') %>% 
  mutate(year_numerical = as.integer(YYYY))

write.csv(all_daily_inf2na_notimputed,
          file.path(tables_output_path,
                    paste0("summary_report_by_completeness_code_for_daily_inf2na_notimputed_",
                           time_period,
                           ".csv")),
          row.names = T)

glimpse(all_daily_inf2na_notimputed)

fx_reportFieldsPar(
  all_daily_inf2na_notimputed,
  all_annual_summaries,

  prefix = time_period,
  suffix = paste0(maxcode, "_classes"),
  output_path = meta_output_path)


source(here("i_spatial_data_preprocessing",
            "0301_visualize_completeness.R"), local=T)

source(here("i_spatial_data_preprocessing",
            "0302_impute_missing_daily_for_all_completeness_criteria.R"), local=T)

cat("03 processes completed at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

cat("Total memory available: ", format(as.numeric(system("vmstat -s | grep 'total memory' | awk '{print $1}'", intern = TRUE)), big.mark = ","), "\n")
cat("Free memory available: ", format(as.numeric(system("vmstat -s | grep 'free memory' | awk '{print $1}'", intern = TRUE)), big.mark = ","), "\n")
cat("Number of physical cores: ", detectCores(logical = FALSE), "\n")
cat("Number of available threads: ", detectCores(logical = TRUE), "\n")