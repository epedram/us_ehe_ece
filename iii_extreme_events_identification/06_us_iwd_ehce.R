## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

### (6) Visualize the impacted geographies (IDW Method) ----
library(here)
library(data.table)
library(tidyverse)

library(sf)
library(sp)
library(stars)
#library(gstat)
#library(ncdf4)
library(tictoc)
library(getopt)
library(doParallel)
sf_use_s2(F)

spec = matrix(c(
  #'state', 's', 1, "character",
  'year'  , 'y', 1, "character",
  'cellsize', 'c', 1, "character",
  'cores', 'p', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)

opt = getopt(spec)

job_id <- opt$job_id

cores <- as.integer(opt$cores)
#cores <- 3 # // [runtime settings] O2 / Cores ==== 

cellsize <- opt$cellsize
#cellsize <-  4000 # // [runtime settings] O2 / Cell ====

nmax = 6 
idp = 2

#nmax = 1L
#idp = 0L

#filename <- file.choose()

pointer <- "archiveX" # // [runtime settings] O2 / Outputs ==== 

project_title <- "06_US_ehe_ece_65_by_stations_idw_mvariables_scidata_"
idw_var <- "EHCMI_minmaxlog"
runtime_params <- "62_"
# // *2 [settings] O2 / Title ====
geography <- paste("US_", #idw_var,
                   "49", #cellsize,
                   sep = "_")
### Geog set 1 ----
geog <- c("California", "CA")
#geog <- c("Massachusetts", "MA")

### Temp set 0 ----
start_year <- opt$year
#start_year <- 2022L # // [runtime settings] O2 / Year====
end_year <-  start_year

### Temp set 1 ----
start_date <-  as.Date(paste0(start_year, "-01-01"), format="%Y-%m-%d")
end_date <-  as.Date(paste0(end_year, "-12-31"), format="%Y-%m-%d")

time_period <- paste(start_year, end_year, sep = "_")
cc_code <-  65L

## Model parameters ----
model_params <- paste("us49", cc_code,
                      "station_id",
                      time_period,
                      "at95_ehcf85",
                      idw_var, 
                      nmax, idp,
                      cellsize, 
                      sep = "_")

task_title <- paste(idw_var, cellsize, nmax, idp, 
                    sep = "_")


#spatial_projection <-  5070 #Albers Equal Area Projection (EPSG code 9822).
#spatial_projection <-  32610 #EPSG:32610 WGS 84 / UTM zone 10N
#spatial_projection <-  4326 #WGS83 used for GPS
spatial_projection <-  3857 # #epsg 3857 that is commonly used for web mapping
spatial_projection_lonlat <- 4269 # CRS that is used to transform lon-lat data (based on NAD 83)

# class(station_points_SF)
# glimpse(station_points_SF)
# 
# dim(station_points_SF)
# sum(!is.na(station_points_SF$elev_m))
# sum(is.na(station_points_SF$elev_m))
# 
# station_points_SP <- as_Spatial(station_points_SF,#us_census_pop,
#                                 cast = TRUE)
# 
# class(station_points_SP)
# 
# station_points_SP_NN <- station_points_SP[!is.na(station_points_SP$"elev_m"), ]
# dim(station_points_SP_NN)

default_crs = sf::st_crs(spatial_projection_lonlat)

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "1_helper_functions_geo.R"), local = T)

source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

# [I/O] Settings -----
inputs_dir <- file.path(source_dir, 
                        "05_noaa_isd_ehf_ecf_ehmi_ehci_ehe_ece_sorted",
                        "R_Objects")

stations_geo_path <- file.path(source_dir,
                        "00_noaa_stations_data_catalog_2007_2022",
                        "us49_stations_2007_2022.rds")

states_file_path <- file.path(source_dir,
                                "00_base_spatial_layers_grids_census_admin_boundaries", 
                                "us49_states_geo_tigris.rds")
states_geo <- readRDS(states_file_path)[[1]]

counties_file_path <- file.path(source_dir,
                                "00_base_spatial_layers_grids_census_admin_boundaries", 
                                "us49_counties_geo_tigris.rds")
counties_geo <- readRDS(counties_file_path)[[1]]

tracts_file_path <- file.path(source_dir,
                              "00_base_spatial_layers_grids_census_admin_boundaries", 
                              "us49_tracts_geo_tigris.rds")
tracts_geo <- readRDS(tracts_file_path)[[1]]

zctas_file_path <- file.path(source_dir,
                            "00_base_spatial_layers_grids_census_admin_boundaries", 
                            #"us49_zctas_geo_states_tigris.rds")
                            "us49_zctas_geo_clipped_tigris.rds") # original

zctas_geo <- readRDS(zctas_file_path)[[1]]

#glimpse(zctas_geo)
climate_regions_geo <- states_geo %>%
  group_by(Climate_Region) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(Climate_Region != "Other") %>% 
  mutate(GEOID = Climate_Region) %>% 
  st_buffer(.,
            dist =  100,
            endCapStyle = "ROUND",
            joinStyle = "ROUND",
            mitreLimit = 1,
            singleSide = FALSE) %>%
  st_zm(., drop=TRUE, what = "ZM") %>% 
  st_transform(spatial_projection) %>% 
  st_cast(., "MULTIPOLYGON")

plot(climate_regions_geo["Climate_Region"])
#glimpse(climate_regions_geo)
#glimpse(states_geo)

# urban_file_path <- file.path(source_dir,
#                              "00_base_spatial_layers_grids_census_admin_boundaries", 
#                              "us49_urban_geo_states_tigris.rds")
# urban_geo <- readRDS(zctas_file_path)[[1]] 

nation_geo <- states_geo %>%
  #group_by(Climate_Region) %>%
  filter(Climate_Region != "Other") %>% 
  summarise(geometry = st_union(geometry)) %>%
  mutate(GEOID = "CONUS") %>% 
  st_buffer(.,
            dist =  100,
            endCapStyle = "ROUND",
            joinStyle = "ROUND",
            mitreLimit = 1,
            singleSide = FALSE) %>%
  st_zm(., drop=TRUE, what = "ZM") %>% 
  st_transform(spatial_projection) %>% 
  st_cast(., "MULTIPOLYGON")

plot(nation_geo["GEOID"])

states_geo_climate <- states_geo %>% st_drop_geometry() %>% 
  dplyr::select(contains("STUSPS") | contains("Climate_Region"))

stations_geo <- readRDS(stations_geo_path) %>%
  st_transform(spatial_projection) %>% 
  distinct(station_name, .keep_all = TRUE) %>% 
  dplyr::select(!YYYY) %>% 
  merge(.,
        states_geo_climate,
        by.x="state",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp"))

#glimpse(stations_geo)

map_divisions <- states_geo

#glimpse(states_geo)
#glimpse(counties_geo)
#glimpse(tracts_geo)
plot(map_divisions[1])

fx_reportFieldsPar(
  states_geo,
  counties_geo,
  tracts_geo,
  zctas_geo,
  stations_geo,
  climate_regions_geo,
  nation_geo,
  
  prefix = "tigris",
  suffix = st_label,
  output_path = meta_output_path)


#### Event variables ----
EHCE_variables <- c(
  "Event_duration",
  "EC", "EH",
  "ECF", "EHF", 
  "ECMI", "EHMI")

tic()
ehe_ece <- fx_combine_rds(inputs_dir, "EHE_ECE_events_only") %>%
  filter(EF_distribution_set %in% c("station_id")) %>%
  #filter(EF_distribution_set %in% c("county_name")) %>%
  filter(completeness_code %in% c("65")) %>%
  mutate(EHCF = EHF + ECF) %>%
  mutate(EHCMI = EHMI + ECMI) %>% 
  mutate(EHC = EH + EC)

#glimpse(ehe_ece)
# compute normalized indicators
ehe_ece <- ehe_ece %>%
  filter(Event_duration > 0) %>% 
  merge(.,
         stations_geo %>% st_drop_geometry(), # first join to apply state info
         by.x="station_id",
         by.y="station_id",
         all.x = TRUE,
         suffix = c("","_sp")) %>% 
  filter(!is.na(station_name)) %>%
  
  ### Geog set 2 ----
  #filter(state %in% geog) %>%
  mutate(EHMI_normalized_by_range_global = fx_normalize_by_range(EHMI),
       ECMI_normalized_by_range_global = fx_normalize_by_range(ECMI)) %>%
  mutate(EHCMI_normalized =
           ECMI_normalized_by_range_global +
           EHMI_normalized_by_range_global) %>%

  mutate(EHF_normalized_by_range_global = fx_normalize_by_range(EHF),
         ECF_normalized_by_range_global = fx_normalize_by_range(ECF)) %>%
  mutate(EHCF_normalized =
           ECF_normalized_by_range_global +
           EHF_normalized_by_range_global) %>% 
  
  mutate(EH_normalized_by_range_global = fx_normalize_by_range(EH),
         EC_normalized_by_range_global = fx_normalize_by_range(EC)) %>%
  mutate(EHC_normalized =
           EC_normalized_by_range_global +
           EH_normalized_by_range_global)

selected_period_geo_events_only <- ehe_ece %>%
  filter(DATE >= start_date &
         DATE <= end_date)
#glimpse(selected_period_geo_events_only)

fx_toc(selected_period_geo_events_only, 0, model_params, "station_based")

summary(ehe_ece$EHF)
summary(ehe_ece$ECF)
#hist(ehe_ece$EHF_normalized_by_range_global)
#hist(ehe_ece$ECF_normalized_by_range_global)

summary(ehe_ece$EH)
summary(ehe_ece$EC)
#hist(ehe_ece$EH_normalized_by_range_global)
#hist(ehe_ece$EC_normalized_by_range_global)

summary(ehe_ece$EHMI)
summary(ehe_ece$ECMI)
#hist(ehe_ece$EHMI_normalized_by_range_global)
#hist(ehe_ece$ECMI_normalized_by_range_global)

# + + + Thresholds ----
EHE_threshold <- quantile(ehe_ece$EHMI_normalized_by_range_global,
                          #ehe_ece$EHF_normalized_by_range_global,
                          #ehe_ece$EH_normalized_by_range_global,
                          probs= 0.50, na.rm=TRUE)

ECE_threshold <- quantile(ehe_ece$ECMI_normalized_by_range_global,
                          #ehe_ece$ECF_normalized_by_range_global,
                          #ehe_ece$EC_normalized_by_range_global,
                          probs= 0.50, na.rm=TRUE)

probs <- c(0, .15, .50, .75, 1)

# EHE_threshold_set <- quantile(ehe_ece$EHMI_normalized_by_range_global,
#                            probs = probs, na.rm=TRUE)
# 
# ECE_threshold_set <- quantile(ehe_ece$ECMI_normalized_by_range_global,
#                                probs = probs, na.rm=TRUE)

unique(ehe_ece$completeness_code)
unique(ehe_ece$EF_distribution_set)

length(unique(ehe_ece$station_id))
length(unique(selected_period_geo_events_only$station_id))

E_DATES <- selected_period_geo_events_only %>% st_drop_geometry() %>%
  dplyr::select(c(DATE, year_number)) %>%
  unique(.) %>%
  arrange(DATE)

masking_layer <- as_Spatial(map_divisions,#us_census_pop,
                             cast = TRUE)

station_points_SF <- stations_geo %>%
  ### Geog set 4 ----
  #filter(state %in% geog) %>%
  st_transform(spatial_projection) 
  
  #glimpse(station_points_SF)
  plot(station_points_SF[4])

st_crs(stations_geo)
st_crs(station_points_SF)

      tic()
      blank_grid_file_path <- file.path(source_dir, 
                                        "00_spatial_grids_sp_sf_stars",
                                        
                                        paste0("blank_grid_stars_clipped_",
                                               cellsize,
                                               "_5xBuffer.rds")
                                        )
      blank_grid <- readRDS(blank_grid_file_path)
      
      #blank_grid <- st_crop(blank_grid,
      #                       states_geo)
        plot(blank_grid)
        plot(masking_layer, add = T, col = "orange")
      fx_toc(blank_grid, 1, start_year, "model_input_stars_clipped")
                                        ### ( + GRID + ) ----
      tic()
       #blank_grid_sp_file_path <- file.path(source_dir,
        #                                 "Grids",
                                         #paste0("blank_grid_SP_clipped_",
                                          #      cellsize,
                                           #     "_from_stars.rds"))
         #                                "blank_grid_SP_500_spsample.rds")
       
      #blank_grid_SP <- readRDS(blank_grid_sp_file_path)
      blank_grid_SP = as(blank_grid, "Spatial")
      
      # calculate the exact number of cells in the idw
      idw_blank <- gstat::idw(z ~ 1,
                                     as_Spatial(states_geo[1] %>% mutate(z = 1) %>% st_centroid(),
                                                cast = TRUE),
                                     newdata = blank_grid_SP,
                                     nmax = nmax,
                                     idp = idp)
      idw_blank_raster <- terra::rast(idw_blank)[[1]]
      crs(idw_blank_raster) <- paste0("epsg:", spatial_projection)
      idw_blank_raster <- crop(idw_blank_raster[[1]], raster::extent(masking_layer))
      idw_blank_raster[(idw_blank_raster < 1)] <- NA 
      
      total_grids <- length(idw_blank_raster[!is.na(idw_blank_raster)])
      
      # blank_grid_stars_clipped <- st_crop(blank_grid,
      #                                     states_geo)
      # length(blank_grid_stars_clipped[[1]])
      # 
        #plot(blank_grid_SP)
        #plot(masking_layer, add = T, col = "yellow")
      
      fx_toc(blank_grid_SP, 1, start_year, "model_input_sp_clipped")
      #ls(pat = "fx_")

   fx_reportFieldsPar(
     stations_geo,
     station_points_SF,
     ehe_ece,
     selected_period_geo_events_only,

     prefix = cellsize,
     suffix = st_label,
     output_path = meta_output_path)

gc()

daily_data_segments <- as.list(E_DATES[[1]])#[7:12] # // [1:1200] ----

E_DATES[[1]]

selected_day <- NULL

  error_date_list <- c()
  error_message_list <- c()
  error_counter_i <- 1
# I/O ====
dir.create(file.path(reports_path , paste0("daily_catalog_boundaries_geo_", cellsize)))
daily_sp_outputs <- file.path(reports_path, paste0("daily_catalog_boundaries_geo_", cellsize))

dir.create(file.path(reports_path , paste0("daily_idw_variables_stacks_", cellsize)))
daily_var_stacks_outputs <- file.path(reports_path, paste0("daily_idw_variables_stacks_", cellsize))

dir.create(file.path(reports_path, paste0("daily_idw_variables_layers_", cellsize)))
daily_var_layers_outputs <- file.path(reports_path, paste0("daily_idw_variables_layers_", cellsize))

dir.create(file.path(reports_path , paste0("daily_idw_ehce_rasters_", cellsize)))
daily_idw_rasters_outputs <- file.path(reports_path, paste0("daily_idw_ehce_rasters_", cellsize))

dir.create(file.path(reports_path , paste0("daily_unified_rasters_", cellsize)))
daily_unified_rasters_outputs <- file.path(reports_path, paste0("daily_unified_rasters_", cellsize))

dir.create(file.path(reports_path , paste0("daily_catalog_impacted_geo_ehce_", cellsize)))
daily_impacted_ehce_outputs <- file.path(reports_path, paste0("daily_catalog_impacted_geo_ehce_", cellsize))

dir.create(file.path(reports_path , paste0("daily_catalog_impacted_geo_stack_", cellsize)))
daily_impacted_stack_outputs <- file.path(reports_path, paste0("daily_catalog_impacted_geo_stack_", cellsize))

dir.create(file.path(reports_path , paste0("viz_impacted_geo_plots_", cellsize)))
impacted_geo_plots_outputs <- file.path(reports_path, paste0("viz_impacted_geo_plots_", cellsize))

dir.create(file.path(reports_path , paste0("viz_idw_ehce_plots_", cellsize)))
idw_outputs <- file.path(reports_path, paste0("viz_idw_ehce_plots_", cellsize))





# {{ + initiate the temporal loop + }} ----
library(doParallel)

# # create a cluster object
cl <- makeCluster(cores, outfile = file.path(runtime_path, paste0("foreach_logs_",
                                                                  start_year,
                                                                  ".txt")))
# register the cluster
registerDoParallel(cl)

foreach(#daily_data = daily_data_segments,
 i = 1:length(daily_data_segments),
       .multicombine = TRUE,
       .verbose = TRUE,
       .packages =c("here", "tictoc", "data.table",
                    "sf", "sp", "raster", "terra", "tidyterra", "stars",
                    "tidyverse", "ggplot2", "logger")
       ) %dopar% {
print(Sys.time())
  
            library(here)
            library(tictoc)
            library(data.table)
            library(tidyverse)
            library(sf)
            library(sp)
            library(terra)
            library(raster)
            library(stars)
            library(tidyterra)
            library(logger)
         
#for(i in 1:length(daily_data_segments)){
  #i <- 1
         logger_file <- file.path(runtime_path, paste0("logger_outputs_",
                                                           start_year,
                                                           ".log"))
         ## define the file logger with log rotation enabled
         log_appender(appender_file(logger_file, max_files = 1L,
                                    max_lines = Inf, max_bytes = Inf))
         
         log_threshold(TRACE)
         log_threshold()
  
  selected_day <- daily_data_segments[[i]]
  
  selected_day_label <- as.character.Date(selected_day)
  core_id <- (i %% cores)
  print(selected_day)
            #}
            
  sf_use_s2(FALSE)
  
  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)
  
  source(here::here("runtime_setup",
                    "1_helper_functions_geo.R"), local = T)
  
  source(here::here("runtime_setup",
                    "2_plotting_functions.R"), local = T)
  
  selected_period_geo_events_only_tmp <- selected_period_geo_events_only %>%
    filter(DATE == selected_day) # %>%
  
  impacted_stations <- length(unique(selected_period_geo_events_only_tmp$station_id))
  #dplyr::select(contains("MI") | contains("id")), # %>% st_drop_geometry()

    station_points_SF_day <-
      merge(station_points_SF, #stations_geo,
            selected_period_geo_events_only_tmp, 
            by.x="station_id",
            by.y="station_id",
            all.x = TRUE,
            suffix = c("","_sp")) %>%
      mutate(EHCMI_normalized = replace_na(EHCMI_normalized, 0))
      #mutate_if(is.numeric, list(~replace_na(., 0))) 
    
    rm(selected_period_geo_events_only_tmp)
    
    station_points_SP <- as_Spatial(station_points_SF_day,
                                    cast = TRUE)
    
    station_points_SP_NN <- station_points_SP[!is.na(station_points_SP[["EHCMI_normalized"]]), ]
    
    tryCatch({ # // . -o-[tryCatch]-o- . ----
      
    # idw for heat/cold indicators ----
    idw_runtime <- fx_ehe_ece_idw(points_df = station_points_SP_NN, 
                               grid_sp = blank_grid,
                               idw_formula = "EHCMI_normalized ~ 1",
                               idp = idp,
                               nmax = nmax)

# plotting iwd surfaces ------
idw_ehce_boundaries_sf <- idw_runtime[[1]]
idw_ehce_terra_raster <- idw_runtime[[2]]
trimmed_grid_stars <- idw_runtime[[3]]
#idw_ehce_raster_unified <- idw_runtime[[4]]

rm(idw_runtime)

    fx_ehe_ece_iwd_plot(idw_outputs, 
                        idw_ehce_terra_raster, 
                        idw_ehce_boundaries_sf,
                        
                        map_divisions,
                        selected_day = selected_day,
                        "EHCMI_normalized")
    
tic()
# interpolated_idw_stack ++++ ----
#ls(station_points_SP@data)
vars_list <- list(
  c("temperature_avg", "temperature_avg ~ 1"),
  c("relative_humidity_avg", "relative_humidity_avg ~ 1"),
  c("wind_speed_avg", "wind_speed_avg ~ 1"),
  ## c("EHCMI", "EHCMI ~ 1"),
  c("EHCMI_normalized", "EHCMI_normalized ~ 1"),
  ## c("EHCF", "EHCF ~ 1"),
  c("EHCF_normalized", "EHCF_normalized ~ 1"),
  ## c("EHC", "EHC ~ 1"),
  c("EHC_normalized", "EHC_normalized ~ 1"),
  c("DM_AT", "DM_AT ~ 1"))

 stacked_vars <- map(vars_list, ~fx_vars_idw(.x[1],
                                .x[2],
                                points_df = station_points_SP,
                                trimmed_grid_stars = trimmed_grid_stars,
                                idp = idp,
                                nmax = nmax))
 rm(trimmed_grid_stars) 
#
 interpolated_idw_stack <- rast(stacked_vars)
 terra::writeRaster(interpolated_idw_stack,
                    file.path(daily_var_stacks_outputs,
                              paste0("daily_idw_variables_stack_",
                                     selected_day, ".tif")),
                    filetype = "GTiff",
                    overwrite=TRUE)
 
 daily_ehcmi_idw_terra_raster <- interpolated_idw_stack["EHCMI_normalized"]
 terra::writeRaster(daily_ehcmi_idw_terra_raster,
                    file.path(daily_var_layers_outputs,
                              paste0("daily_ehcmi_idw_",
                                     selected_day, ".tif")),
                    filetype = "GTiff",
                    overwrite=TRUE)
 
 daily_at_idw_terra_raster <- interpolated_idw_stack["DM_AT"]
 terra::writeRaster(daily_at_idw_terra_raster,
                    file.path(daily_var_layers_outputs,
                              paste0("daily_at_idw_",
                                     selected_day, ".tif")),
                    filetype = "GTiff",
                    overwrite=TRUE)
 
 daily_t_idw_terra_raster <- interpolated_idw_stack["temperature_avg"]
 terra::writeRaster(daily_t_idw_terra_raster,
                    file.path(daily_var_layers_outputs,
                              paste0("daily_t_idw_",
                                     selected_day, ".tif")),
                    filetype = "GTiff",
                    overwrite=TRUE)
 
fx_toc(interpolated_idw_stack, 1, selected_day_label, "stacked_idw_vars")

fx_ehe_ece_iwd_plot(idw_outputs, 
                    daily_at_idw_terra_raster, 
                    idw_ehce_boundaries_sf,
                    
                    map_divisions,
                    selected_day = selected_day,
                    "DM_AT")

rm(interpolated_idw_stack)

# EHE/ECE augmentation (with admin/census boundaries) -----
 idw_terra_rasters <- list(daily_t_idw_terra_raster, daily_at_idw_terra_raster, daily_ehcmi_idw_terra_raster)
 idw_vars <- c("temperature", "apparent_temperature", "ehcmi_normalized")
 
 tic() ## ehe_ece x states ----
 admin_geo_label <- "States"
 states_impacts <- fx_impacted_geo(states_geo, idw_ehce_boundaries_sf, 
                                   idw_ehce_terra_raster,
                                   idw_var,
                                   admin_geo_label,
                                   model_params)
 
 polygons <- states_impacts[[1]]
 
  fx_ehe_ece_admin_geo_plot(polygons, 
                           idw_outputs,
                           admin_geo_label,
                           selected_day = selected_day)
 
 fx_toc(states_impacts, 0, selected_day_label, admin_geo_label)
 
 saveRDS(polygons,
         file.path(daily_impacted_ehce_outputs,
                   paste0(admin_geo_label,
                          "_geo_ehe_ece_",
                          selected_day,
                          ".rds")))
 
 polygons_vars_stack <- fx_impacted_geo_multi(states_geo, idw_ehce_boundaries_sf, 
                                      idw_terra_rasters,
                                      idw_vars,
                                      admin_geo_label,
                                      model_params)[[1]]
 
 saveRDS(polygons_vars_stack,
         file.path(daily_impacted_stack_outputs,
                   paste0(admin_geo_label,
                          "_impacted_area_variables_stack_",
                          selected_day,
                          ".rds")))
 rm(states_impacts)
 
 
gc()
tic() ## ehe_ece x counties  ----
admin_geo_label <- "Counties"
counties_impacts <- fx_impacted_geo(counties_geo, idw_ehce_boundaries_sf, 
                                    idw_ehce_terra_raster, 
                                    idw_var,
                                    admin_geo_label,
                                    model_params)
    polygons <- counties_impacts[[1]]

saveRDS(polygons,
        file.path(daily_impacted_ehce_outputs,
                  paste0(admin_geo_label,
                         "_geo_ehe_ece_",
                            selected_day,
                                ".rds")))

fx_ehe_ece_admin_geo_plot(polygons, 
                          idw_outputs,
                          admin_geo_label,
                          selected_day = selected_day)

 fx_toc(counties_impacts, 0, selected_day_label, admin_geo_label)
 
 
 polygons_vars_stack <- fx_impacted_geo_multi(counties_geo, idw_ehce_boundaries_sf, 
                                              idw_terra_rasters,
                                              idw_vars,
                                              admin_geo_label,
                                              model_params)[[1]]
 
 saveRDS(polygons_vars_stack,
         file.path(daily_impacted_stack_outputs,
                   paste0(admin_geo_label,
                          "_impacted_area_variables_stack_",
                          selected_day,
                          ".rds")))
rm(counties_impacts)


tic() ## ehe_ece x tracts ----
admin_geo_label <- "Tracts" 
tracts_impacts <- fx_impacted_geo(tracts_geo, idw_ehce_boundaries_sf, 
                                  idw_ehce_terra_raster,
                                  idw_var,
                                  admin_geo_label,
                                  model_params)

    polygons <- tracts_impacts[[1]]
    
    fx_ehe_ece_admin_geo_plot(polygons, 
                             idw_outputs,
                             admin_geo_label,
                             selected_day = selected_day)

fx_toc(tracts_impacts, 0, selected_day_label, admin_geo_label)

saveRDS(polygons,
        file.path(daily_impacted_ehce_outputs,
                  paste0(admin_geo_label,
                         "_geo_ehe_ece_",
                            selected_day,
                                ".rds")))
polygons_vars_stack <- fx_impacted_geo_multi(tracts_geo, idw_ehce_boundaries_sf, 
                                             idw_terra_rasters,
                                             idw_vars,
                                             admin_geo_label,
                                             model_params)[[1]]

saveRDS(polygons_vars_stack,
        file.path(daily_impacted_stack_outputs,
                  paste0(admin_geo_label,
                         "_impacted_area_variables_stack_",
                         selected_day,
                         ".rds")))
rm(tracts_impacts)


tic() ## ehe_ece x zipcode ----
admin_geo_label <- "ZCTAs" 
zctas_impacts <- fx_impacted_geo(zctas_geo, idw_ehce_boundaries_sf, 
                                  idw_ehce_terra_raster,
                                  idw_var,
                                  admin_geo_label,
                                  model_params)

polygons <- zctas_impacts[[1]]

fx_ehe_ece_admin_geo_plot(polygons, 
                          idw_outputs,
                          admin_geo_label,
                          selected_day = selected_day)

fx_toc(zctas_impacts, 0, selected_day_label, admin_geo_label)

saveRDS(polygons,
        file.path(daily_impacted_ehce_outputs,
                  paste0(admin_geo_label,
                         "_geo_ehe_ece_",
                         selected_day,
                         ".rds")))

polygons_vars_stack <- fx_impacted_geo_multi(zctas_geo, idw_ehce_boundaries_sf, 
                                             idw_terra_rasters,
                                             idw_vars,
                                             admin_geo_label,
                                             model_params)[[1]]

saveRDS(polygons_vars_stack,
        file.path(daily_impacted_stack_outputs,
                  paste0(admin_geo_label,
                         "_impacted_area_variables_stack_",
                         selected_day,
                         ".rds")))
rm(zctas_impacts)


tic() ## ehe_ece x climate regions ----
admin_geo_label <- "Climate_Regions"
climate_regions_impacts <- fx_impacted_geo(climate_regions_geo, idw_ehce_boundaries_sf,
                                  idw_ehce_terra_raster,
                                  idw_var,
                                  admin_geo_label,
                                  model_params)

polygons <- climate_regions_impacts[[1]]

fx_ehe_ece_admin_geo_plot(polygons,
                          idw_outputs,
                          admin_geo_label,
                          selected_day = selected_day)

fx_toc(climate_regions_impacts, 0, selected_day_label, admin_geo_label)

saveRDS(polygons,
        file.path(daily_impacted_ehce_outputs,
                  paste0(admin_geo_label,
                         "_geo_ehe_ece_",
                         selected_day,
                         ".rds")))

polygons_vars_stack <- fx_impacted_geo_multi(climate_regions_impacts, idw_ehce_boundaries_sf,
                                             idw_terra_rasters,
                                             idw_vars,
                                             admin_geo_label,
                                             model_params)[[1]]

saveRDS(polygons_vars_stack,
        file.path(daily_impacted_stack_outputs,
                  paste0(admin_geo_label,
                         "_impacted_area_variables_stack_",
                         selected_day,
                         ".rds")))
rm(climate_regions_impacts)


tic() ## ehe_ece x nation ----
admin_geo_label <- "Nation"
nation_impacts <- fx_impacted_geo(nation_geo, idw_ehce_boundaries_sf,
                                           idw_ehce_terra_raster,
                                           idw_var,
                                           admin_geo_label,
                                           model_params)

polygons <- nation_impacts[[1]]

fx_ehe_ece_admin_geo_plot(polygons,
                          idw_outputs,
                          admin_geo_label,
                          selected_day = selected_day)

fx_toc(nation_impacts, 0, selected_day_label, admin_geo_label)

saveRDS(polygons,
        file.path(daily_impacted_ehce_outputs,
                  paste0(admin_geo_label,
                         "_geo_ehe_ece_",
                         selected_day,
                         ".rds")))

polygons_vars_stack <- fx_impacted_geo_multi(nation_impacts, idw_ehce_boundaries_sf,
                                             idw_terra_rasters,
                                             idw_vars,
                                             admin_geo_label,
                                             model_params)[[1]]

saveRDS(polygons_vars_stack,
        file.path(daily_impacted_stack_outputs,
                  paste0(admin_geo_label,
                         "_impacted_area_variables_stack_",
                         selected_day,
                         ".rds")))
rm(nation_impacts)

# read the idw post-processing script and apply visualization function 
# source(here::here("extreme_events_identification",
#                   "0801_idw_post_processing.R"), local = T)
# # 
print(ls(pattern = "idw_ehce_"))

print(gc())

    }, error = function(e){
      log_debug(selected_day)
      log_warn("idw error")
      log_error(conditionMessage(e))
    })
    
    print(Sys.time())
}

print(showConnections())
print("Closing")
print(stopCluster(cl))
gc()

source(here("ii_extreme_events_identification",
            "0601_compile_yearly_results.R"), local=T)
