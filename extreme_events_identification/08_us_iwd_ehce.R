## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

### (8) Visualize the impacted geographies (IDW Method) ----

library(here)
library(data.table)
library(tidyverse)

library(sf)
#library(ncdf4)
library(sp)
#library(gstat)
library(tictoc)
library(getopt)
library(doParallel)


### Temp set 0 ----
spec = matrix(c(
  #'state', 's', 1, "character",
  'year'  , 'y', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)
opt = getopt(spec)

job_id <- opt$job_id
project_title <- "08_isd_ehe_ece"
task_title <- "idw_interpolation"
### Temp set 1 ----
geography <- paste("us_", sep = "_")

### * 1 parameterized yearly period ----
#start_year <- opt$year #2022L 
start_year <- 2022L 
end_year <-  start_year
start_date <-  as.Date(paste0(start_year, "-01-01"), format="%Y-%m-%d")
end_date <-  as.Date(paste0(end_year, "-12-31"), format="%Y-%m-%d")

### Geog set 1 ----
#geog <- c("California", "CA")
#geog <- c("Massachusetts", "MA")

year <- paste(start_year, end_year, sep = "_")

## Model parameters ----
cellsize <-  500
EHE_threshold <- 0.0
ECE_threshold <- -0.1

EHCE_threshold <- 0.1
idw <-  2

#spatial_projection <-  5070 #Albers Equal Area Projection (EPSG code 9822).
#spatial_projection <-  32610 #EPSG:32610 WGS 84 / UTM zone 10N

#spatial_projection <-  4326 #WGS83 used for GPS
spatial_projection <-  3857 # #epsg 3857 that is commonly used for web mapping
spatial_projection_lonlat <- 4269 # CRS that is used to transform lon-lat data (based on NAD 83)

default_crs = sf::st_crs(spatial_projection_lonlat)


source(here::here("runtime_setup",
                  "0_io.R"), local = T)

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

#dir.create(file.path(scratch_dir, paste0("ehe_ece_stations_hyperparameters_plots_")))
#output_path <- reports_path

# I/O Settings -----
inputs_dir <- file.path(source_dir, "05_noaa_isd_identified_ehe_ece")

#### EHCE_variables ----
EHCE_variables <- c(
  "Event_duration",
  "EHF", "EHMI",
  "ECF", "ECMI")

code_labels <- c("70" = "All stations (missing values imputed)",
                 "73" = "All stations (short gaps imputed)",
                 "85" = "85% completeness (missing values imputed)",
                 "95" = "95% completeness (missing values imputed)")

stations_file_path <- file.path(source_dir,
                                "03_us_imputed_isd_daily_2008_2022",
                                "noaa_isd_stations_contiguous_us_2007_2022.rds")

stations_geo <- readRDS(stations_file_path)%>%
  st_transform(spatial_projection)

ehe_ece <- fx_combine_rds(inputs_dir, "EHE_ECE_events_only") %>%
  filter(EF_distribution_set %in% c("station_id")) %>%
  filter(completeness_code %in% c("73")) %>%
  mutate(cc_label = recode_factor(as.character(completeness_code), !!!code_labels)) %>%
  mutate(EHCF = EHF + ECF) %>%
  mutate(EHCMI = EHMI + ECMI)

#hist(ehe_ece$EHCMI)

glimpse(ehe_ece)

selected_period_geo_events_only <- ehe_ece %>%
   merge(.,
         stations_geo %>% st_drop_geometry(), # first join to apply state info
         by.x="station_id",
         by.y="station_id",
         all.x = TRUE,
         suffix = c("","_sp")) %>%
   #st_as_sf() %>%
  ### Geog set 2 ----
  #filter(state %in% geog) %>%
  mutate(EHMI_normalized_by_range_global = fx_normalize_by_range(EHMI),
         ECMI_normalized_by_range_global = fx_normalize_by_range(ECMI)) %>%
  mutate(EHCMI_normalized =
           ECMI_normalized_by_range_global +
           EHMI_normalized_by_range_global) %>%
  filter(DATE >= start_date &
           DATE <= end_date)

glimpse(selected_period_geo_events_only)

E_DATES <- selected_period_geo_events_only %>% st_drop_geometry() %>%
  dplyr::select(c(DATE, YYYY)) %>%
  unique(.) %>%
  arrange(DATE)

saveRDS(selected_period_geo_events_only,
        file.path(rds_output_path,
                  "selected_period_geo_events_only_2008_2022_73cc_stations.rds"))

summary(selected_period_geo_events_only$EHMI_normalized)
summary(selected_period_geo_events_only$ECMI_normalized)

EHCE_threshold <- quantile(selected_period_geo_events_only$EHCMI_normalized,
                           probs= 0.1, na.rm=TRUE)

probs <- c(0, .15, .85, 1)

EHCE_thresholds_set <- quantile(selected_period_geo_events_only$EHCMI_normalized,
                           probs= probs, na.rm=TRUE)

unique(ehe_ece$completeness_code)
unique(ehe_ece$EF_distribution_set)

length(unique(ehe_ece$station_id))
length(unique(selected_period_geo_events_only$station_id))


us_census_pop <- readRDS(file.path(source_dir,
                   "03_us_imputed_isd_daily_2008_2022",
                   "us_census_pop_2019.rds"))

masking_layer <- as_Spatial(us_census_pop,
                             cast = TRUE)

station_points_SF <- stations_geo %>%
  ### Geog set 4 ----
  #filter(state %in% geog) %>%
  st_transform(spatial_projection)
  #glimpse(station_points_SF)
  plot(station_points_SF[4])

st_crs(stations_geo)
st_crs(station_points_SF)

  # # Replace point boundary extent with that of the state
  # station_points_SP@bbox <- masking_layer@bbox

  # Create an empty grid where n is the total number of cells
  #   tic()
### Geog set 5 ----
  #   blank_grid_SP              <- as.data.frame(sp::spsample(masking_layer, "regular",
  #                                              cellsize = cellsize,
  #                                              n = 0,
  #                                              crs = spatial_projection_lonlat))
  #   names(blank_grid_SP)       <- c("X", "Y")
  #   coordinates(blank_grid_SP) <- c("X", "Y")
  #   gridded(blank_grid_SP)     <- TRUE  # Create SpatialPixel object
  #   fullgrid(blank_grid_SP)    <- TRUE  # Create SpatialGrid object
  # ## Add P's projection information to the empty grid
  #   proj4string(blank_grid_SP) <- proj4string(masking_layer)
  # timing_sp <- toc()$callback_msg
  # grdmsize_sp <- object.size(blank_grid_SP)
  # cat("\nBlank grid by Stars \n - processing time:", timing_sp,
  # "\n - object size: ", format(grdmsize_sp, units = "auto"),
  # "\n - object type: ", class(blank_grid_SP))
  #  saveRDS(blank_grid_SP,
  #          file.path(rds_output_path,
  #                    "CA_blank_grid_SP.rds"))
  #
      blank_grid_file_path <- file.path(source_dir,
                                        "03_us_imputed_isd_daily_2008_2022",
                                        #"CA_blank_grid_SP.rds")
                                        #"CA_blank_grid_SP_masked.rds")
                                        "blank_grid_SP.rds")
      blank_grid_SP <- readRDS(blank_grid_file_path)

      ls(pat = "fx_")
      
      #fx_info(blank_grid_SP)
   #print(blank_grid_SP[1]@grid)

   tic()
   plot(blank_grid_SP)
   plot(masking_layer, add = T, col = "yellow")
   fx_toc(blank_grid_SP)

   fx_reportFieldsPar(
     stations_geo,
     station_points_SF,
     ehe_ece,
     selected_period_geo_events_only,
     us_census_pop,

     prefix = cellsize,
     suffix = st_label,
     output_path = meta_output_path)

gc()
#hist(EHCE_selected_period_geo$EHMI_normalized_by_range)
#summary(EHCE_selected_period_geo$EHMI_normalized_by_range)
# f(x): Create a named list of objects based on the original objects name
# https://stackoverflow.com/questions/18861772/r-get-objects-names-from-the-list-of-objects

#summary(NamedList)

daily_data_segments <- as.list(E_DATES[[1]])
E_DATES[[1]]
daily_data_segments <- as.list(E_DATES[[1]])
selected_day <- NULL

  error_date_list <- c()
  error_message_list <- c()
  error_counter_i <- 1

dir.create(file.path(runtime_path , paste0(start_year, "_Method_IWD_", cellsize)))
idw_outputs <- file.path(runtime_path, paste0(start_year, "_Method_IWD_", cellsize))


# # Initiate the temporal loop ----
library(doParallel)
cores <- 4
# # create a cluster object
cl <- makeCluster(cores)

# # register the cluster
registerDoParallel(cl)

foreach(daily_data = daily_data_segments, .verbose = TRUE) %dopar% {

  selected_day <<- daily_data[[1]]
  print(selected_day)

  library(here)
  library(sf)
  library(sp)
  library(tidyverse)
  library(tictoc)
  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)

    station_points_SF_day <-
      merge(stations_geo,
              selected_period_geo_events_only %>%
              #st_transform(spatial_projection) %>%
              filter(DATE == selected_day), # %>%
              #dplyr::select(contains("MI") | contains("id")), # %>% st_drop_geometry(),
            by.x="station_id",
            by.y="station_id",
            all.x = TRUE,
            suffix = c("","_sp")) %>%
      mutate_if(is.numeric, list(~replace_na(., 0)))
    #glimpse(station_points_SF_day)

    station_points_SP <- as_Spatial(station_points_SF_day,
                                    cast = TRUE)
    #plot(station_points_SF_day[2])
    #plot(station_points_SP)
    
### SP idw ----
gc()
tic()
#replicate(10,
#interpolated_idw_SP <- gstat::idw(EHCMI ~ 1, EHCMI_normalized
interpolated_idw_SP <- gstat::idw(EHCF ~ 1, # interpolation variable ----
                                      station_points_SP,
                                      newdata = blank_grid_SP, # base grid
                                      nmax = 6 ,
                                      idp = idw)
#)
fx_toc(interpolated_idw_SP)


    tryCatch({

    # read the idw post-processing script and apply visualization function
      source(here::here("extreme_events_identification",
                        "0801_idw_post_processing.R"), local = T)
      
      ls(pattern = "idw_ehce_")

      print(gc())
      
    }, error = function(e){

      cat(selected_day,
          "ERROR :",conditionMessage(e), "\n")

      error_date_list <<- append(error_date_list,
                                 as.character(selected_day))
      error_message_list <<- append(error_message_list,
                                    as.character(conditionMessage(e)))
      error_counter_i <<- error_counter_i + 1
    })

 }

print(showConnections())
print("Closing")
print(stopCluster(cl))
