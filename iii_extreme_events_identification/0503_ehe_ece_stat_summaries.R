# title: "Spatiotemporal Delineation of Extreme Heat/Cold Events"
## subtitle: "Spatial Model Comparison"

## Model parameters ----
##```{r}
source(here::here("validation", "runtime_conf.R"), local=T)
##```

## I/O ----
##```{r set IO parameters, include=FALSE}
library(here)

project_name <- "Validation_"

input_dir <- "~/data_interim/processed_data/"

output_dir <- "~/ws_outputs/"

timestamped <- format(Sys.time(), "%m%d_%H%M")

print(Sys.time)
print(timestamped)

timestamped_folder <- paste0(project_name ,timestamped)

dir.create(file.path(output_dir, timestamped_folder))
output_path <- paste0(output_dir, timestamped_folder)

print(output_dir)
print(output_path)

# dir.create(file.path(output_path, "Method_1_Nearest_Station"))
# dir.create(file.path(output_path, "Method_2_Contour_Overlay"))
# dir.create(file.path(output_path, "Methods_Comparison"))
dir.create(file.path(output_path, "RDS_Archive"))

# M1_output_path <- file.path(output_path, "Method_1_Nearest_Station")
# M2_output_path <- file.path(output_path, "Method_2_Contour_Overlay")
# MC_output_path <- file.path(output_path, "Methods_Comparison")
rds_output_path <- file.path(output_path, "RDS_Archive")
##```


## Libraries and functions ----
##```{r libs}
source(here("R", "libraries.R"))

source(here("R", "helper_functions.R"))

writeLines(capture.output(sessionInfo()),
           paste0(output_path, "/_",
                  timestamped, "_session_lib_info.txt"))
##```


## Set the graphical theme
##```{r}
new <-  theme_classic() + theme(
        axis.text.x = element_text(angle=45 ,hjust=1),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        legend.box = "vertical",
        legend.key.height = unit(1.2, "mm"),
        legend.position="none"
        #strip.text.x = element_blank(),
        #strip.text.y = element_blank()
        )

theme_set(new)
##```
# Load the input data ----
##```{r}
noaa_points_sf <- readRDS(here(input_dir,
                                  "noaa_isd_2021_noaa_isd_sf_ca_state_100km.rds"))[[1]] %>%
  rename_at("station_name", ~ "Station_Name")

noaa_stations_climate_dt <- readRDS(here(input_dir,
                                         "stations_daily_means_compiled_sf.rds"))[[1]] %>%
  filter(DATE >= start_date & DATE <= end_date) %>%
  filter(!is.na(dm_apparent_temperature))


# CA_voronoi_polygons_sf <- readRDS(here(input_dir,
#                                  "CA_voronoi_polygons_sf.rds"))[[1]]
#
# SCA_voronoi_polygons_sf <- readRDS(here(input_dir,
#                                  "SCA_voronoi_polygons_sf.rds"))[[1]]
#
# CA_fixed_grid_sf <- readRDS(here(input_dir,
#                                  "CA_fixed_grid_sf.rds"))[[1]]

CA_counties_sf <- readRDS(here(input_dir,
                                 "CA_counties_sf_ACS2019.rds"))[[1]]

CA_tracts_sf <- readRDS(here(input_dir,
                                 "CA_tracts_sf_ACS2019.rds"))[[1]]

# CA_blocks_sf <- readRDS(here(input_dir,
#                                  "CA_blocks_sf_ACS2019.rds"))[[1]]
#
# CA_zipcodes_sf <- readRDS(here(input_dir,
#                                   "CA_zipcodes_sf_ACS2019.rds"))[[1]]

# EHE <- readRDS(here(input_dir,
#                                  "EHE.rds"))[[1]]
#
# ECE <- readRDS(here(input_dir,
#                                  "ECE.rds"))[[1]]

EHE_ECE_compiled <- readRDS(here(input_dir,
                                 "EHE_ECE_compiled.rds"))[[1]]

EHE_ECE <- readRDS(here(input_dir,
                                 "EHE_ECE.rds"))[[1]]

glimpse(EHE_ECE)

class(EHE_ECE)

EHE_ECE_compiled <- EHE_ECE %>%
  filter(., !is.na(Event_Type))

  #group_by(UID)

glimpse(EHE_ECE_compiled)
##```

# Stats
EHE_ECE_Aggregate_Annual_Stats <- EHE_ECE_compiled %>%
  group_by(Event_Type) %>% ##Summary by event type -----
  summarise(Number_of_Records = n(),
            Distinct_Stations = n_distinct(station_id),

            Average_EHE_Duration = round(mean(EHE_duration, na.rm = TRUE), 2),
            Max_EHE_Duration = max(EHE_duration, na.rm = TRUE),
            Average_EHF = round(mean(EHF, na.rm = TRUE), 1),
            Max_EHF = round(max(EHF, na.rm = TRUE), 1),

            Average_EHMI = round(mean(EHMI, na.rm = TRUE), 1),
            Max_EHMI = round(max(EHMI, na.rm = TRUE), 1),

            Average_ECE_Duration = round(mean(ECE_duration, na.rm = TRUE), 2),
            Max_ECE_Duration = max(ECE_duration, na.rm = TRUE),
            Average_ECF = round(mean(ECF, na.rm = TRUE), 1),
            Min_ECF = round(min(ECF, na.rm = TRUE), 1),

            Average_ECMI = round(mean(ECMI, na.rm = TRUE), 1),
            Min_ECMI = round(min(ECMI, na.rm = TRUE), 1),

            Average_AT = round(mean(DM_apparent_temperature, na.rm = TRUE),2),
            Min_AT = min(DM_apparent_temperature, na.rm = TRUE),
            Max_AT = max(DM_apparent_temperature, na.rm = TRUE),
            Median_AT = round(median(DM_apparent_temperature, na.rm = TRUE), 2)
            )
          # Parcels_Total_Area_per_Zone = sum((!!area_field)),
          # Parcels_Mean_Area_per_Zone = mean(!!area_field, na.rm = TRUE)) %>%

EHE_ECE_Aggregate_Monthtly_Stats <- EHE_ECE_compiled %>%
  group_by(Event_Type, Month) %>% ##Summary by event type and month-----
  summarise(Number_of_Records = n(),
            Distinct_Stations = n_distinct(station_id),

            Average_EHE_Duration = round(mean(EHE_duration, na.rm = TRUE), 2),
            Max_EHE_Duration = max(EHE_duration, na.rm = TRUE),
            Average_EHF = round(mean(EHF, na.rm = TRUE), 1),
            Max_EHF = round(max(EHF, na.rm = TRUE), 1),

            Average_EHMI = round(mean(EHMI, na.rm = TRUE), 1),
            Max_EHMI = round(max(EHMI, na.rm = TRUE), 1),

            Average_ECE_Duration = round(mean(ECE_duration, na.rm = TRUE), 2),
            Max_ECE_Duration = max(ECE_duration, na.rm = TRUE),
            Average_ECF = round(mean(ECF, na.rm = TRUE), 1),
            Min_ECF = round(min(ECF, na.rm = TRUE), 1),

            Average_ECMI = round(mean(ECMI, na.rm = TRUE), 1),
            Min_ECMI = round(min(ECMI, na.rm = TRUE), 1),

            Average_AT = round(mean(DM_apparent_temperature, na.rm = TRUE),2),
            Min_AT = min(DM_apparent_temperature, na.rm = TRUE),
            Max_AT = max(DM_apparent_temperature, na.rm = TRUE),
            Median_AT = round(median(DM_apparent_temperature, na.rm = TRUE), 2)
            )

EHE_ECE_Stations_Stats <- EHE_ECE_compiled %>%
  group_by(station_id, Event_Type) %>% ##Summary by station and event type -----
  summarise(Number_of_Records = n(),
            Distinct_Events = n_distinct(UID),

            Average_EHE_Duration = round(mean(EHE_duration, na.rm = TRUE), 2),
            Max_EHE_Duration = max(EHE_duration, na.rm = TRUE),
            Average_EHF = round(mean(EHF, na.rm = TRUE), 1),
            Max_EHF = round(max(EHF, na.rm = TRUE), 1),

            Average_EHMI = round(mean(EHMI, na.rm = TRUE), 1),
            Max_EHMI = round(max(EHMI, na.rm = TRUE), 1),

            Average_ECE_Duration = round(mean(ECE_duration, na.rm = TRUE), 2),
            Max_ECE_Duration = max(ECE_duration, na.rm = TRUE),
            Average_ECF = round(mean(ECF, na.rm = TRUE), 1),
            Min_ECF = round(min(ECF, na.rm = TRUE), 1),

            Average_ECMI = round(mean(ECMI, na.rm = TRUE), 1),
            Min_ECMI = round(min(ECMI, na.rm = TRUE), 1),

            Average_AT = round(mean(DM_apparent_temperature, na.rm = TRUE),2),
            Min_AT = min(DM_apparent_temperature, na.rm = TRUE),
            Max_AT = max(DM_apparent_temperature, na.rm = TRUE),
            Median_AT = round(median(DM_apparent_temperature, na.rm = TRUE), 2)
            )

write.csv(EHE_ECE_Aggregate_Annual_Stats,
          paste0(output_path, "/",
                 deparse(substitute(EHE_ECE_Aggregate_Annual_Stats)),
                 ".csv"),
          row.names = FALSE, )

write.csv(EHE_ECE_Aggregate_Monthtly_Stats,
          paste0(output_path, "/",
                 deparse(substitute(EHE_ECE_Aggregate_Monthtly_Stats)),
                 ".csv"),
          row.names = FALSE, )

write.csv(EHE_ECE_Stations_Stats,
          paste0(output_path, "/",
                 deparse(substitute(EHE_ECE_Stations_Stats)),
                 ".csv"),
          row.names = FALSE, )


write.csv(dm_apparent_temperature_percentiles,
          paste0(output_path, "/",
                 deparse(substitute(dm_apparent_temperature_percentiles)),

                 nrow(dm_apparent_temperature_percentiles),
                 "stations",
                 ".csv"),
          row.names = FALSE, )

write.csv(EHF_percentiles,
          paste0(output_path, "/",
                 deparse(substitute(EHF_percentiles)),

                 nrow(EHF_percentiles),
                 "stations",
                 ".csv"),
          row.names = FALSE, )

write.csv(ECF_percentiles,
          paste0(output_path, "/",
                 deparse(substitute(ECF_percentiles)),

                 nrow(ECF_percentiles),
                 "stations",
                 ".csv"),
          row.names = FALSE, )

write.csv(ECE,
          paste0(output_path, "/",
                 deparse(substitute(ECE)),

                 nrow(ECE),
                 "station_x_day" ,
                 ".csv"),
          row.names = FALSE, )

write.csv(EHE,
          paste0(output_path, "/",
                 deparse(substitute(EHE)),

                 nrow(EHE),
                 "station_x_day" ,
                 ".csv"),
          row.names = FALSE, )

write.csv(EHE_ECE,
          paste0(output_path, "/",
                 deparse(substitute(EHE_ECE)),

                 nrow(EHE_ECE),
                 "station_x_day" ,
                 ".csv"),
          row.names = FALSE, )
