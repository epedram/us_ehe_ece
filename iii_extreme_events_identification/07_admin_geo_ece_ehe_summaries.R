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
library(tictoc)
library(getopt)
library(logger)

sf_use_s2(F)

spec = matrix(c(
  #'state', 's', 1, "character",
  'cellsize', 'c', 1, "character",
  'year'  , 'y', 1, "character",
  'job_id'  , 'j', 1, "character"
), byrow=TRUE, ncol=4)
opt = getopt(spec)

cellsize <- opt$cellsize
cellsize <-  4000

job_id <- opt$job_id
pointer <- "archive"

idw_var <- "ehcmi__minmaxlog"
cc_code <-  65L

### Temp set 0 ----
start_year <- opt$year
start_year <- 2008 
end_year <-  start_year + 14
### Temp set 1 ----
start_date <-  as.Date(paste0(start_year, "-01-01"), format="%Y-%m-%d")
end_date <-  as.Date(paste0(end_year, "-12-31"), format="%Y-%m-%d")
#start_date <-  as.Date(paste0(start_year, "-02-23"), format="%Y-%m-%d")
#start_date <-  as.Date(paste0(start_year, "-01-06"), format="%Y-%m-%d")
#end_date <-  as.Date(paste0(end_year, "-01-30"), format="%Y-%m-%d")

time_period <- paste(start_year, end_year, sep = "_")

model_params <- paste("us49", cc_code,
                      "counties",
                      time_period,
                      "at95_ehcf85",
                      idw_var
                      )

# // *1 [settings] O2 / Title ====
project_title <- "07_US_ehe_ece_catalog_65_by_stations_multivariables_scidata"

idw_var <- "EHCMI_minmaxlog"
runtime_params <- "62"

task_title <- paste0(#model_params, 
                     "aggregated_from_", cellsize)

geography <- paste("US_", idw_var,
                   "_idw_", cellsize,
                   sep = "_")
### Geog set 1 ----
geog <- c("California", "CA")
#geog <- c("Massachusetts", "MA")


## Model parameters ----
idw <-  2

#spatial_projection <-  5070 #Albers Equal Area Projection (EPSG code 9822).
#spatial_projection <-  32610 #EPSG:32610 WGS 84 / UTM zone 10N

#spatial_projection <-  4326 #WGS83 used for GPS
spatial_projection <-  3857 # #epsg 3857 that is commonly used for web mapping
spatial_projection_lonlat <- 4269 # CRS that is used to transform lon-lat data (based on NAD 83)

default_crs = sf::st_crs(spatial_projection_lonlat)

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

source(here::here("runtime_setup",
                  "1_helper_functions_geo.R"), local = T)

source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

source(here::here("runtime_setup",
                  "0_io_us.R"), local = T)

dir.create(file.path(runtime_path, paste0("study_period_cumulative")))
cumulative_outputs_path <- file.path(runtime_path, paste0("study_period_cumulative"))

## create a folder storing the log files
logger_file <- file.path(meta_output_path, paste0("logger_outputs_",
                                                  task_title,
                                                  ".log"))

log_appender(appender_file(logger_file, max_files = 1L,
                           max_lines = Inf, max_bytes = Inf))

log_threshold(TRACE)
log_threshold()

# I/O Settings -----
inputs_dir <- file.path(#source_dir, 
                        scratch_dir,
                        paste0("06_US_ehe_ece_",
                        cc_code, # // *2 [settings] O2 / input runtime title ====
                        "_by_stations_idw_mvariables_scidata__EHCMI_minmaxlog_4000_6_2"))			

list.files(inputs_dir)

## Event variables ----
EHCE_variables <- c(
  "Event_duration",
  "EC", "EH",
  "ECF", "EHF", 
  "ECMI", "EHMI")

states_file_path <- file.path(source_dir,
                              "00_base_spatial_layers_grids_census_admin_boundaries", 
                              "us49_states_geo_tigris.rds")
states_geo <- readRDS(states_file_path)[[1]]
states_geo_climate <- states_geo %>% st_drop_geometry() %>% dplyr::select(contains("STUSPS") | contains("Climate_Region"))


counties_file_path <- file.path(source_dir,
                                "00_base_spatial_layers_grids_census_admin_boundaries", 
                                "us49_counties_geo_tigris.rds")
counties_geo <- readRDS(counties_file_path)[[1]] %>% 
  merge(.,
        states_geo_climate,
        by.x="STUSPS",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp"))


tracts_file_path <- file.path(source_dir,
                              "00_base_spatial_layers_grids_census_admin_boundaries", 
                              "us49_tracts_geo_tigris.rds")
tracts_geo <- readRDS(tracts_file_path)[[1]] %>% 
  merge(.,
        states_geo_climate,
        by.x="STUSPS",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp"))


zctas_file_path <- file.path(source_dir,
                             "00_base_spatial_layers_grids_census_admin_boundaries", 
                             "us49_zctas_geo_states_tigris.rds")
zctas_geo <- readRDS(zctas_file_path)[[1]] %>% 
  merge(.,
        states_geo_climate,
        by.x="STUSPS",
        by.y="STUSPS",
        all.x = TRUE,
        suffix = c("","_sp"))

climate_regions_geo <- states_geo %>%
  group_by(Climate_Region) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(Climate_Region != "Other") %>% 
  mutate(GEOID = Climate_Region) %>% 
  st_buffer(.,
            dist =  1000,
            endCapStyle = "ROUND",
            joinStyle = "ROUND",
            mitreLimit = 1,
            singleSide = FALSE) %>%
  st_zm(., drop=TRUE, what = "ZM") %>% 
  st_transform(spatial_projection) %>% 
  st_cast(., "MULTIPOLYGON")

plot(climate_regions_geo["Climate_Region"])


fx_reportFieldsPar(
  states_geo,
  counties_geo,
  tracts_geo,
  zctas_geo,
  climate_regions_geo,

  prefix = "augmented",
  suffix = st_label,
  output_path = meta_output_path)

map_divisions <- states_geo

plot(map_divisions[1])

col_names <- c("GEOID", "STUSPS", "STATE_NAME", "NAME", "Climate_Region")
#glimpse(states_geo)
#glimpse(zctas_geo)
#glimpse(tracts_geo)

stations_geo_path <- file.path(source_dir,
                               "00_noaa_stations_data_catalog_2007_2022",
                               "us49_stations_2007_2022.rds")
station_points_SF <- readRDS(stations_geo_path)
#plot(station_points_SF[1])
# { ( ( ( BASE LAYER FOR SPATIAL AGGREGATION ) ) ) } ====
## Raster processing ====
yearly_dataset_path <- file.path(inputs_dir,
                                  paste0("yearly_datasets_", cellsize))

list.files(yearly_dataset_path)

tryCatch({
  tic()  ### ** combining yearly ehce unified rasters  ----
  cumualtive_ehce_unified_grids <- fx_combine_yearly_rasters(yearly_dataset_path, 
                                                             paste0("ehce_unified_grids_",
                                                                    cellsize, "_cumulative"),
                                                             
                                                             cumulative_outputs_path,
                                                             
                                                             paste0("cumulative",
                                                                    "_ehce_unified_grids_",
                                                                    cellsize),
                                                             
                                                             idw_var_name = "EHCE",
                                                             plot_ranges = c(0, 250, 50))
  
  fx_toc(cumualtive_ehce_unified_grids, 1, time_period, cellsize)
  rm(cumualtive_ehce_unified_grids)
}, error = function(e){
  log_warn("ehe compiling error")
  log_error(conditionMessage(e))
})

tryCatch({
  tic()  ### ** combining yearly ehe unified rasters  ----
  cumualtive_ehe_unified_grids <- fx_combine_yearly_rasters(yearly_dataset_path, 
                                                             paste0("ehe_unified_grids_",
                                                                    cellsize, "_cumulative"),
                                                             
                                                             cumulative_outputs_path,
                                                             
                                                             paste0("cumulative",
                                                                    "_ehe_unified_grids_",
                                                                    cellsize),
                                                             
                                                             idw_var_name = "EHE",
                                                            plot_ranges = c(0, 150, 30))
  
  fx_toc(cumualtive_ehe_unified_grids, 1, time_period, cellsize)
  rm(cumualtive_ehe_unified_grids)
}, error = function(e){
  log_warn("ehe compiling error")
  log_error(conditionMessage(e))
})

tryCatch({
  tic()  ### ** combining yearly ece unified rasters  ----
  cumualtive_ece_unified_grids <- fx_combine_yearly_rasters(yearly_dataset_path, 
                                                             paste0("ece_unified_grids_",
                                                                    cellsize, "_cumulative"),
                                                             
                                                            cumulative_outputs_path,
                                                             
                                                             paste0("cumulative",
                                                                    "_ece_unified_grids_",
                                                                    cellsize),
                                                             
                                                             idw_var_name = "ECE",
                                                             plot_ranges = c(0, 150, 30))
  
  fx_toc(cumualtive_ece_unified_grids, 1, time_period, cellsize)
  rm(cumualtive_ece_unified_grids)
}, error = function(e){
  log_warn("ehe compiling error")
  log_error(conditionMessage(e))
})


list.files(yearly_dataset_path, "_ehe_ece_boundaries_")
tic()  ## ** combining yearly ehe/ece boundaries sf  ----

ehe_ece_boundaries <- fx_combine_rds(yearly_dataset_path, 
                                     paste0("_ehe_ece_boundaries_", cellsize, ".rds"))

fx_toc(ehe_ece_boundaries, 1, time_period, cellsize)
glimpse(ehe_ece_boundaries)

st_write(ehe_ece_boundaries,
         file.path(cumulative_outputs_path,
                   paste0(start_year,
                          "_ehe_ece_boundaries_",
                          cellsize,
                          ".gpkg")),
         #                           ".geojson")),
         delete_layer = TRUE)

## Vector processing ====
runtime_results_path <- file.path(inputs_dir,
                                  paste0("daily_catalog_impacted_geo_stack_", cellsize))

list.files(runtime_results_path)

tic()

# admin_geo_label <- "Climate_Regions" # Climate_Regions ----
# compiled_objects <- fx_combine_yearly_rds(runtime_results_path, 
#                                           admin_geo_label, 
#                                           climate_regions_geo,
#                                           col_names)
# 
# conus_cregion_events_catalog_2008_2022_dt <- compiled_objects[[1]]
# 
# cregion_summary_2008_2022 <- compiled_objects[[3]]
# 
# cregion_summary_2008_2022_sf <- compiled_objects[[4]]
# 
# fx_toc(conus_cregion_events_catalog_2008_2022_dt, 1, admin_geo_label, time_period)
# 
# gc()
# 
# 
# length(unique(tracts_geo$GEOID))
# length(unique(tracts_summary_2008_2022_sf$GEOID))
# #unique(conus_counties_events_catalog_2008_2022$)
# 
# #hist(states_summary_2008_2022_sf$total_event_days)
# # hist(counties_summary_2008_2022_sf$total_event_days)
# # hist(tracts_summary_2008_2022_sf$total_event_days)
# 
# fx_viz_agg_15years_plot(cregion_summary_2008_2022_sf, "Climate_Regions")
# params_suffix <- "Climate_Regions"
# #fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
# fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
# fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "year_numerical", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
# rm(cregion_summary_2008_2022_sf)

gc()

# Spatial aggregation -----
tic()
#ls(states_geo)
admin_geo_label <- "States" ## States ----
compiled_objects <- fx_combine_yearly_rds(runtime_results_path, 
                                          admin_geo_label,
                                          states_geo,
                                          col_names)
# 
ls(compiled_objects[[1]])
ls(compiled_objects[[2]])
# glimpse(compiled_objects)
# glimpse(states_geo[, colnames(states_geo) %in% col_names])
conus_states_events_catalog_2008_2022_dt <- compiled_objects[[1]]


conus_states_events_catalog_2008_2022_sf <- compiled_objects[[2]]
# compiled_admin_geo_ehe_ece_sf <- conus_states_events_catalog_2008_2022_dt %>% 
#   #right_join(admin_geo_sf["GEOID"],
#   right_join(states_geo[, colnames(states_geo) %in% col_names],
#              .,
#              by="GEOID") %>% 
#   drop_na(event_type) %>% st_as_sf()

#ls(compiled_admin_geo_ehe_ece_sf)
#ls(conus_states_events_catalog_2008_2022_dt)
#ls(compiled_objects[[1]])
#include_cols <- names(select_if(conus_states_events_catalog_2008_2022_dt, is.numeric)) # // . ----[histoboxplot]---- . ----

# class(conus_states_events_catalog_2008_2022_sf)
# length(unique(conus_states_events_catalog_2008_2022_sf$GEOID))
# glimpse(conus_states_events_catalog_2008_2022_sf)
#
# #setDT(conus_states_events_catalog_2008_2022)
# #class(conus_states_events_catalog_2008_2022)
#
 states_summary_2008_2022 <- compiled_objects[[3]]
# glimpse(states_summary_2008_2022)
#
 states_summary_2008_2022_sf <- compiled_objects[[4]]
# class(states_summary_2008_2022_sf)
# glimpse(states_summary_2008_2022_sf)

#plot(states_summary_2008_2022_sf["total_event_days"])
fx_toc(conus_states_events_catalog_2008_2022_dt, 1, admin_geo_label, time_period)
ls(conus_states_events_catalog_2008_2022_dt)

### Visualization --------
fx_viz_agg_15years_plot(states_summary_2008_2022_sf, "States")
#rm(states_summary_2008_2022_sf)

params_suffix <- admin_geo_label
#fx_histboxplot(conus_states_events_catalog_2008_2022_sf, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_states_events_catalog_2008_2022_dt, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_states_events_catalog_2008_2022_dt, "year_numerical", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----

tic()
admin_geo_label <- "Counties" ## Counties ----
compiled_objects <- fx_combine_yearly_rds(runtime_results_path, 
                                          admin_geo_label, 
                                          counties_geo,
                                          col_names)
conus_counties_events_catalog_2008_2022_dt <- compiled_objects[[1]]


# class(conus_counties_events_catalog_2008_2022_sf)
# length(unique(conus_counties_events_catalog_2008_2022_sf$GEOID))
# glimpse(conus_counties_events_catalog_2008_2022_sf)
# 
# #setDT(conus_counties_events_catalog_2008_2022)
# 
 counties_summary_2008_2022 <- compiled_objects[[3]]
# glimpse(counties_summary_2008_2022)
# 
 counties_summary_2008_2022_sf <- compiled_objects[[4]]
# 
# #plot(counties_summary_2008_2022_sf["total_event_days"])

fx_toc(conus_counties_events_catalog_2008_2022_dt, 1, admin_geo_label, time_period)
glimpse(counties_summary_2008_2022_sf)


# export gpkg for testing shiny app
st_write(counties_summary_2008_2022_sf,
         file.path(reports_path,
                   paste0("counties_summary_2008_2022_sf",
                          ".gpkg")),
         delete_layer = TRUE)

data_ehe <- counties_summary_2008_2022_sf %>% 
  dplyr::filter(event_type == "Extreme Heat Event")
max(data_ehe$total_event_days)

data_ece <- counties_summary_2008_2022_sf %>% 
  dplyr::filter(event_type == "Extreme Cold Event")

max(data_ece$total_event_days)
dim(data_ehe)
dim(data_ece)
#hist(data_ece$event_type)

params_suffix <- admin_geo_label
fx_viz_agg_15years_plot(counties_summary_2008_2022_sf, "Counties")
#rm(counties_summary_2008_2022_sf)
#fx_histboxplot(conus_counties_events_catalog_2008_2022_dt, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_counties_events_catalog_2008_2022_dt, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_counties_events_catalog_2008_2022_dt, "year_numerical", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----

dim(counties_geo)
dim(counties_summary_2008_2022_sf)

tic()
admin_geo_label <- "Tracts" ## Tracts ----
compiled_objects <- fx_combine_yearly_rds(runtime_results_path, 
                                          admin_geo_label, 
                                          tracts_geo,
                                          col_names)

conus_tracts_events_catalog_2008_2022_dt <- compiled_objects[[1]]

# class(conus_tracts_events_catalog_2008_2022_sf)
# length(unique(conus_tracts_events_catalog_2008_2022_sf$GEOID))
# glimpse(conus_tracts_events_catalog_2008_2022_sf)
# 
# #setDT(conus_tracts_events_catalog_2008_2022)
# 
 tracts_summary_2008_2022 <- compiled_objects[[3]]
# glimpse(tracts_summary_2008_2022)
# 
 tracts_summary_2008_2022_sf <- compiled_objects[[4]]
# class(tracts_summary_2008_2022_sf)
# glimpse(tracts_summary_2008_2022_sf)
#plot(counties_summary_2008_2022_sf["total_event_days"])
fx_toc(conus_tracts_events_catalog_2008_2022_dt, 1, admin_geo_label, time_period)

ls(tracts_geo)
fx_viz_agg_15years_plot(tracts_summary_2008_2022_sf, "Tracts")
params_suffix <- "Tracts"
#fx_histboxplot(conus_tracts_events_catalog_2008_2022_dt, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_tracts_events_catalog_2008_2022_dt, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_tracts_events_catalog_2008_2022_dt, "year_numerical", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
#rm(tracts_summary_2008_2022_sf)

tic()
admin_geo_label <- "ZCTAs" ## ZCTAs ----
compiled_objects <- fx_combine_yearly_rds(runtime_results_path, 
                                          admin_geo_label, 
                                          zctas_geo,
                                          col_names)

conus_zctas_events_catalog_2008_2022_dt <- compiled_objects[[1]]

zctas_summary_2008_2022 <- compiled_objects[[3]]

zctas_summary_2008_2022_sf <- compiled_objects[[4]]
# class(tracts_summary_2008_2022_sf)
# glimpse(zctas_summary_2008_2022_sf)
#plot(counties_summary_2008_2022_sf["total_event_days"])
fx_toc(conus_zctas_events_catalog_2008_2022_dt, 1, admin_geo_label, time_period)


gc()

#plot(conus_states_events_catalog_2008_2022[1])

#dim(states_geo)
#dim(states_summary_2008_2022_sf)

# length(unique(states_geo$GEOID))
# length(unique(states_summary_2008_2022_sf$GEOID))

length(unique(counties_geo$GEOID))
length(unique(counties_summary_2008_2022_sf$GEOID))

length(unique(tracts_geo$GEOID))
length(unique(tracts_summary_2008_2022_sf$GEOID))
#unique(conus_counties_events_catalog_2008_2022$)

#hist(states_summary_2008_2022_sf$total_event_days)
# hist(counties_summary_2008_2022_sf$total_event_days)
# hist(tracts_summary_2008_2022_sf$total_event_days)

fx_viz_agg_15years_plot(zctas_summary_2008_2022_sf, "ZCTAs")
params_suffix <- "ZCTAs"
#fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
##fx_histboxplot(conus_zctas_events_catalog_2008_2022_dt, "year_numerical", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
rm(zctas_summary_2008_2022_sf)
gc()
# 
# fx_reportFieldsPar(
#   conus_states_events_catalog_2008_2022_dt,
#   conus_counties_events_catalog_2008_2022_dt,
#   conus_tracts_events_catalog_2008_2022_dt,
#   conus_zctas_events_catalog_2008_2022_dt,
# #  conus_urban_events_catalog_2008_2022_dt,
#   
#   prefix = time_period,
#   suffix = paste0("from_", cellsize),
#   output_path = distplot_outputs_path)


params_suffix <- paste0("US65", "from_", cellsize)

col_list <- c("avg_intensity", 
              "median_intensity",
              "max_intensity",
              "min_intensity")

vfx_histocomparison_scales(conus_counties_events_catalog_2008_2022_dt,
                    conus_states_events_catalog_2008_2022_dt,
                    sc1_label = "counties",
                    sc2_label = "states",
                    col_list,
                    geo_output_path,
                    params_suffix)

vfx_histocomparison_scales(conus_counties_events_catalog_2008_2022_dt,
                           conus_tracts_events_catalog_2008_2022_dt,
                           sc1_label = "counties",
                           sc2_label = "tracts",
                           col_list,
                           geo_output_path,
                           params_suffix)

vfx_histocomparison_scales(conus_tracts_events_catalog_2008_2022_dt,
                           conus_tracts_events_catalog_2008_2022_dt,
                           sc1_label = "tracts",
                           sc2_label = "tracts",
                           col_list,
                           geo_output_path,
                           params_suffix)

# vfx_histocomparison_scales(conus_states_events_catalog_2008_2022_dt,
#                            conus_states_events_catalog_2008_2022_dt,
#                            sc1_label = "states",
#                            sc2_label = "states",
#                            col_list,
#                            geo_output_path,
#                            params_suffix)
# 
# vfx_histocomparison_scales(conus_counties_events_catalog_2008_2022_dt,
#                            conus_counties_events_catalog_2008_2022_dt,
#                            sc1_label = "counties",
#                            sc2_label = "counties",
#                            col_list,
#                            geo_output_path,
#                            params_suffix)

log_info(paste0("Data processing completed!"))
