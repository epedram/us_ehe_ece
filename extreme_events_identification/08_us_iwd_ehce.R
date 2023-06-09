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
                                #"03_us_imputed_isd_daily_2008_2022",
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
                   #"03_us_imputed_isd_daily_2008_2022",
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

      blank_grid_file_path <- file.path(source_dir,
                                        #"03_us_imputed_isd_daily_2008_2022",
                                        #"CA_blank_grid_SP.rds")
                                        #"CA_blank_grid_SP_masked.rds")
                                        "blank_grid_SP.rds")
      blank_grid_SP <- readRDS(blank_grid_file_path)

      ls(pat = "fx_")

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

daily_data_segments <- as.list(E_DATES[[1]])

E_DATES[[1]]

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
cl <- makeCluster(cores, outfile = "~/outfile.txt")

# # register the cluster
registerDoParallel(cl)

foreach(#daily_data = daily_data_segments, 
  i = 1:length(daily_data_segments),
        .multicombine = TRUE,
        .verbose = TRUE,
        .packages =c("here", "sf", "sp", "data.table", "tidyverse", "tictoc", 
                     "terra", "ggplot2", "tidyterra")
        ) %dopar% {

#for(daily_data in daily_data_segments){
  
  selected_day <- daily_data_segments[[i]]
  print(selected_day)

  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)
  
  selected_period_geo_events_only_tmp <- selected_period_geo_events_only %>%
    filter(DATE == selected_day) # %>%
  #dplyr::select(contains("MI") | contains("id")), # %>% st_drop_geometry()

    tryCatch({
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
    #plot(station_points_SF_day[2])
    #plot(station_points_SP)
    
### SP idw ----
gc()
tic()

#interpolated_idw_SP <- gstat::idw(EHCMI ~ 1, EHCMI_normalized
interpolated_idw_SP <- gstat::idw(EHCF ~ 1, # interpolation variable ----
                                      station_points_SP,
                                      newdata = blank_grid_SP, # base grid
                                      nmax = 6 ,
                                      idp = idw)
fx_toc(interpolated_idw_SP)

# Create the Raster object that is needed for computing cumulative & differential stats, not suitable for visualization
tic()
# IWD 2 Raster ----
idw_ehce_raster_bbox <- raster::raster(interpolated_idw_SP["var1.pred"])
names(idw_ehce_raster_bbox) <- selected_day

saveRDS(idw_ehce_raster_bbox,
        file.path(rds_output_path,
                  paste0("idw_ehce_raster_bbox_",
                         selected_day,
                         ".rds")))

fx_toc(idw_ehce_raster_bbox)

tic()

idw_ehce_raster <- raster::mask(raster::crop(idw_ehce_raster_bbox,
                                             masking_layer), masking_layer)
rm(idw_ehce_raster_bbox)

idw_ehce_raster <- trim(idw_ehce_raster)

saveRDS(idw_ehce_raster,
        file.path(idw_outputs,
                  paste0("idw_ehce_raster_masked_",
                         selected_day,
                         ".rds")))

terra::writeRaster(idw_ehce_raster,
                   file.path(geo_output_path,
                             paste0("idw_ehce_raster_masked_",
                                    selected_day)),
                   filetype = "raster",
                   overwrite=TRUE)

fx_toc(idw_ehce_raster)

tic()
idw_ehce_dt <- as.data.frame(idw_ehce_raster, xy=TRUE)
setDT(idw_ehce_dt)

saveRDS(idw_ehce_dt,
        file.path(rds_output_path,
                  paste0("idw_ehce_dt_",
                         selected_day,
                         ".rds")))

table_path <- file.path(tables_output_path, 
                        paste0("idw_ehce_dt_",
                               selected_day,
                               ".csv"))

#write.table(idw_ehce_dt, table_path, 
#            sep = ",", row.names = F, col.names = T,
#            append = F)

fx_toc(idw_ehce_dt)
rm(idw_ehce_dt) ## remove data.table object ----



tic()
# Create the Raster object that is needed for visualization
# IDW 2 Terra Raster ----
idw_ehce_terra_raster <- terra::rast(interpolated_idw_SP["var1.pred"])
idw_ehce_terra_raster[(idw_ehce_terra_raster == 0)] <- NA
idw_ehce_terra_raster <- mask(idw_ehce_terra_raster, us_census_pop)
names(idw_ehce_terra_raster) <- selected_day

saveRDS(idw_ehce_terra_raster,
        file.path(rds_output_path,
                  paste0("idw_ehce_terra_raster_",
                         selected_day,
                         ".rds")))

terra::writeRaster(idw_ehce_terra_raster,
                   file.path(geo_output_path,
                             paste0("idw_ehce_terra_raster_",
                                    selected_day, ".tif")),
                   filetype = "GTiff",
                   overwrite=TRUE)


fx_toc(idw_ehce_terra_raster)

tic()

# # IDW 2 Stars ----
# interpolated_idw_stars <- stars::st_as_stars(idw_ehce_raster)
# saveRDS(interpolated_idw_stars,
#         file.path(rds_output_path,
#                   paste0("interpolated_idw_stars_",
#                          selected_day,
#                          ".rds")))
# fx_toc(interpolated_idw_stars)
# 
# rm(interpolated_idw_stars) ## remove stars object ----
rm(idw_ehce_raster) ## remove raster masked object ----



# Visualization functions----
fx_ehe_ece_iwd_plot <- function(terralayer, output_path,
                                selected_day = selected_day,
                                caption = "Data Source: NOAA Integrated Surface Database (ISD)") {
  
  interpolated_EHCE_plot <-  ggplot() +
    geom_spatraster(data = terralayer) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, na.value = "white") +
    #
  geom_sf(data = us_census_pop,
          fill = NA,
          color = "Black",
          lwd = .5) +
    
    labs(title = paste0("Extreme Event Date: ",  selected_day)) +
    labs(subtitle = paste0("Contiguous U.S.")) +# (", year, ")")) +
    labs(caption = caption) +
    

    geom_sf(data = station_points_SF,
            color = "Black",
            size = .6) # +
  #theme_blank()
  
  plot_file_name <- paste0(plots_output_path, "/",
                           "ehe_ece_iwd_",
                           #"_contour_overlay_",
                           selected_day,
                           ".jpg")
  
  tic()
  ggsave(plot_file_name,
         plot = interpolated_EHCE_plot,
         dpi = 300,
         width = 32, height = 24, units = "cm")
  fx_toc(interpolated_EHCE_plot)
}

fx_ehe_ece_iwd_plot(idw_ehce_terra_raster, 
                    idw_outputs,
                    selected_day = selected_day)
rm(idw_ehce_terra_raster) ## remove terra raster object ----

tic()
# # IDW 2 SF ----
interpolated_idw_SF <- interpolated_idw_SP %>% st_as_sf() %>%
  rename_at(vars(contains("var1.pred")), ~"Estimated_Level") %>%
  filter(Estimated_Level != 0) %>%
  mutate(Estimated_Level = round(Estimated_Level, 2)) %>%
  mutate(DATE = selected_day)
# 
saveRDS(interpolated_idw_SF,
        file.path(rds_output_path,
                  paste0("interpolated_idw_SF_",
                         selected_day,
                         ".rds")))
fx_toc(interpolated_idw_SF)

rm(interpolated_idw_SF) ## remove sf object ----
rm(interpolated_idw_SP) ## remove sp object ----
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
