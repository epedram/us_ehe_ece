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


spec = matrix(c(
  #'state', 's', 1, "character",
  'year'  , 'y', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)
opt = getopt(spec)

job_id <- opt$job_id
project_title <- "08_isd_ehe_ece"
task_title <- "idw_interpolation"
geography <- paste("us_4cores_", sep = "_")

### Temp set 0 ----
start_year <- opt$year
#start_year <- 2022L 
end_year <-  start_year
### Temp set 1 ----
start_date <-  as.Date(paste0(start_year, "-01-01"), format="%Y-%m-%d")
end_date <-  as.Date(paste0(end_year, "-12-31"), format="%Y-%m-%d")
#start_date <-  as.Date(paste0(start_year, "-02-23"), format="%Y-%m-%d")
#end_date <-  as.Date(paste0(end_year, "-02-27"), format="%Y-%m-%d")

### Geog set 1 ----
geog <- c("California", "CA")
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
                                #"03_us_imputed_isd_daily_2008_2022", # // ----
                                "noaa_isd_stations_contiguous_us_2007_2022.rds")

stations_geo <- readRDS(stations_file_path)%>%
  st_transform(spatial_projection)
tic()
ehe_ece <- fx_combine_rds(inputs_dir, "EHE_ECE_events_only") %>%
  filter(EF_distribution_set %in% c("station_id")) %>%
  filter(completeness_code %in% c("73")) %>%
  mutate(cc_label = recode_factor(as.character(completeness_code), !!!code_labels)) %>%
  mutate(EHCF = EHF + ECF) %>%
  mutate(EHCMI = EHMI + ECMI)

#hist(ehe_ece$EHCMI)

#glimpse(ehe_ece)

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

#glimpse(selected_period_geo_events_only)
fx_toc(selected_period_geo_events_only, 0,"73imputation", "station_based")

saveRDS(selected_period_geo_events_only,
        file.path(rds_output_path,
                  "selected_period_geo_events_only_2008_2022_73cc_stations.rds"))

E_DATES <- selected_period_geo_events_only %>% st_drop_geometry() %>%
  dplyr::select(c(DATE, YYYY)) %>%
  unique(.) %>%
  arrange(DATE)


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
                   #"03_us_imputed_isd_daily_2008_2022",  # // ----
                   "us_census_pop_2019.rds")) %>% 
                    ### Geog set 3 ----
                    #filter(state_name %in% geog) %>% 
                   st_transform(spatial_projection)

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

      blank_grid_file_path <- file.path(source_dir,
                                        #"03_us_imputed_isd_daily_2008_2022",  # // ----
                                        #"CA_blank_grid_SP.rds")
                                        #"CA_blank_grid_SP_masked.rds")
                                        "blank_grid_SP.rds")
                                        ### Geog set 5 ----
      blank_grid_SP <- readRDS(blank_grid_file_path)

      #ls(pat = "fx_")

   tic()
   plot(blank_grid_SP)
   plot(masking_layer, add = T, col = "yellow")
   fx_toc(blank_grid_SP, 1, start_year, "model_input")

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

daily_data_segments <- as.list(E_DATES[[1]])

E_DATES[[1]]

selected_day <- NULL

  error_date_list <- c()
  error_message_list <- c()
  error_counter_i <- 1

dir.create(file.path(runtime_path , paste0(start_year, "_Method_IWD_", cellsize)))
idw_outputs <- file.path(runtime_path, paste0(start_year, "_Method_IWD_", cellsize))


# Initiate the temporal loop ----
library(doParallel)
cores <- 4

# # create a cluster object
cl <- makeCluster(cores, outfile = file.path(runtime_path, "foreach_logs.txt"))

# # register the cluster
registerDoParallel(cl)

 foreach(#daily_data = daily_data_segments, 
   i = 1:length(daily_data_segments),
         .multicombine = TRUE,
         .verbose = TRUE,
         .packages =c("here", "sf", "sp", "data.table", "tidyverse", "tictoc", 
                      "terra", "ggplot2", "tidyterra")
         ) %dopar% {

#for(i in 1:length(daily_data_segments)){
  selected_day <- daily_data_segments[[i]]
  selected_day_label <- as.character.Date(selected_day)
  core_id <- (i %% cores)
  print(selected_day)
            #}
            library(here)
            library(tictoc)
            library(data.table)
            library(tidyverse)
            library(sf)
            library(sp)
            library(raster)
            library(terra)
            library(tidyterra)
            library(stars)
  
  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)
  
  selected_period_geo_events_only_tmp <- selected_period_geo_events_only %>%
    filter(DATE == selected_day) # %>%
  
  impacted_stations <- length(unique(selected_period_geo_events_only_tmp$station_id))
  #dplyr::select(contains("MI") | contains("id")), # %>% st_drop_geometry()

    station_points_SF_day <-
      merge(stations_geo,
            selected_period_geo_events_only_tmp, 
            by.x="station_id",
            by.y="station_id",
            all.x = TRUE,
            suffix = c("","_sp")) %>%
      mutate_if(is.numeric, list(~replace_na(., 0)))
    
    rm(selected_period_geo_events_only_tmp)
    
    station_points_SP <- as_Spatial(station_points_SF_day,
                                    cast = TRUE)
    #plot(station_points_SP)

    ## SP input idw ----

tic()
#interpolated_idw_SP <- gstat::idw(EHCMI ~ 1, EHCMI_normalized
interpolated_idw_SP <- gstat::idw(EHCF ~ 1, ## Set the interpolation variable ----
                                      station_points_SP,
                                      newdata = blank_grid_SP, # base grid
                                      nmax = 6,
                                      idp = idw)
#  rgdal::writeOGR(interpolated_idw_SP,
#                  dsn = file.path(idw_outputs),
#                  layer = paste0("interpolated_idw_SP_",
#                            selected_day),
# ##                 layer_options ="wkbPolygon",
#                  verbose = FALSE,
#                  delete_dsn = TRUE, overwrite_layer = TRUE,
#                  driver = "GPKG") # not working

fx_toc(interpolated_idw_SP, 1, selected_day_label, core_id)

tryCatch({
      
# Create the Raster object that is needed for computing cumulative & differential stats, not suitable for visualization
tic()
#### IWD 2 Raster ----
idw_ehce_raster_bbox <- raster::raster(interpolated_idw_SP["var1.pred"])
names(idw_ehce_raster_bbox) <- selected_day

# saveRDS(idw_ehce_raster_bbox,
#         file.path(rds_output_path,
#                   paste0("idw_ehce_raster_bbox_",
#                          selected_day,
#                          ".rds")))

idw_ehce_raster <- raster::mask(raster::crop(idw_ehce_raster_bbox,
                                             masking_layer), masking_layer)
rm(idw_ehce_raster_bbox)
idw_ehce_raster <- raster::trim(idw_ehce_raster)

terra::writeRaster(idw_ehce_raster,
                   file.path(geo_output_path,
                             paste0("idw_ehce_raster_masked_",
                                    selected_day)),
                   filetype = "raster",
                   overwrite=TRUE)
fx_toc(idw_ehce_raster, 1, selected_day_label, core_id)

tic()
#### Raster 2 Contour----
tryCatch({
  idw_ehce_contour <- rasterToContour(idw_ehce_raster,
                                      nlevels = 7,
                                      maxpixels=10000000
                                      ) %>%  
                      st_as_sf(.,
                               crs = spatial_projection)
}, error = function(e){
  idw_ehce_contour <- idw_ehce_contour[0,]
  
  cat("ERROR :",conditionMessage(e), "\n")
  return(idw_ehce_contour)
})
fx_toc(idw_ehce_contour, 1, selected_day_label, core_id)


tic()
idw_ehce_dt <- as.data.frame(idw_ehce_raster, xy=TRUE)

setDT(idw_ehce_dt)

# table_path <- file.path(tables_output_path, 
#                         paste0("idw_ehce_dt_",
#                                selected_day,
#                                ".csv"))
# 
# write.table(idw_ehce_dt, table_path, 
#             sep = ",", row.names = F, col.names = T,
#             append = F)

fx_toc(idw_ehce_dt, 1, selected_day_label, core_id)
rm(idw_ehce_dt) #### remove data.table object ----


tic()
# Create the Raster object that is needed for visualization
#### IDW 2 Terra Raster ----
idw_ehce_terra_raster <- terra::rast(interpolated_idw_SP["var1.pred"])
idw_ehce_terra_raster[(idw_ehce_terra_raster == 0)] <- NA
idw_ehce_terra_raster <- mask(idw_ehce_terra_raster, us_census_pop)
names(idw_ehce_terra_raster) <- selected_day

terra::writeRaster(idw_ehce_terra_raster,
                   file.path(geo_output_path,
                             paste0("idw_ehce_terra_raster_",
                                    selected_day, ".tif")),
                   filetype = "GTiff",
                   overwrite=TRUE)

# writeCDF(idw_ehce_terra_raster, 
#          file.path(idw_outputs, "netcdf.nc"),
#          varname=paste0("layer_", selected_day), 
#          overwrite=F)
fx_toc(idw_ehce_terra_raster, 1, selected_day_label, core_id)

tic()
#### IDW 2 SF ----
idw_ehce_sf <- interpolated_idw_SP %>% st_as_sf() %>%
  rename_at(vars(contains("var1.pred")), ~"Estimated_Level") %>%
  filter(Estimated_Level != 0) %>%
  mutate(Estimated_Level = round(Estimated_Level, 2)) %>%
  mutate(DATE = selected_day)

# st_write(idw_ehce_sf,
#          file.path(geo_output_path,
#                    paste0("idw_ehce_sf_",
#                           selected_day,
#                           ".gpkg")),
#          delete_layer = TRUE) # works and creates large files

fx_toc(idw_ehce_sf, 1, selected_day_label, core_id)

#### IDW 2 Terra Vector  ----
tic()
idw_ehce_terra_vector <- terra::vect(idw_ehce_sf)

# terra::writeVector(idw_ehce_terra_vector,
#                    file.path(geo_output_path,
#                              paste0("idw_ehce_terra_vector_",
#                                     selected_day)),
#                    overwrite=TRUE) #works, creates shapefile

fx_toc(idw_ehce_terra_vector, 1, selected_day_label, core_id)
rm(idw_ehce_terra_vector)


tic()
#### IDW 2 Stars ----
idw_ehce_stars <- stars::st_as_stars(idw_ehce_sf)

fx_toc(idw_ehce_stars, 1, selected_day_label, core_id)
rm(idw_ehce_sf) #### remove sf object ----
rm(idw_ehce_stars) #### remove stars object ----

tic()
#### Raster 2 Events Boundaries ----
idw_ehce_raster_unified <- idw_ehce_raster

#idw_ehce_raster_unified[(idw_ehce_raster_unified > ECE_threshold) & (idw_ehce_raster_unified < EHE_threshold)] <- NA
idw_ehce_raster_unified[(idw_ehce_raster_unified == 0)] <- NA
idw_ehce_raster_unified[!is.na(idw_ehce_raster_unified)] <- 1L
names(idw_ehce_raster_unified) <- selected_day

idw_ehce_unified_polygon <- rasterToPolygons(clump(idw_ehce_raster_unified)) %>%
   st_as_sf() %>% 
   mutate(DATE = as.Date(selected_day, "%a, %d %b %Y")) %>%
  mutate(Estimated_Level = 1) %>%
  mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
  mutate(area_hectare = round((computed_area / 10000), 1))

# st_write(idw_ehce_unified_polygon,
#          file.path(idw_outputs,
#                    paste0("idw_ehce_unified_polygon_",
#                           selected_day,
#                           ".gpkg")),
#          delete_layer = TRUE) # works and create large files

fx_toc(idw_ehce_unified_polygon, 1, selected_day_label, core_id)
rm(idw_ehce_raster_unified)
rm(idw_ehce_unified_polygon)

rm(idw_ehce_raster) #### remove raster masked object ----


fx_ehe_ece_iwd_plot(idw_ehce_terra_raster, 
                    idw_outputs,
                    impacted_stations,
                    selected_day = selected_day)
rm(idw_ehce_terra_raster) #### remove terra raster object ----
rm(interpolated_idw_SP) #### remove sp object ----

gc()

    # read the idw post-processing script and apply visualization function 
      # source(here::here("extreme_events_identification",
      #                   "0801_idw_post_processing.R"), local = T)
      # # 
      ls(pattern = "idw_ehce_")

      print(gc())
      
    }, error = function(e){

      cat(selected_day,
          "ERROR :",conditionMessage(e), "\n")

      error_date_list <- append(error_date_list,
                                 as.character(selected_day))
      error_message_list <- append(error_message_list,
                                    as.character(conditionMessage(e)))
      error_counter_i <- error_counter_i + 1
    })

 }

print(showConnections())
print("Closing")
print(stopCluster(cl))
