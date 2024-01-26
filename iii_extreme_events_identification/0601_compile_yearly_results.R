## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

dir.create(file.path(reports_path , paste0("yearly_datasets_", cellsize)))
yearly_dataset_path <- file.path(reports_path, paste0("yearly_datasets_", cellsize))

library(sf)
library(sp)
library(terra)
library(raster)
library(logger)

source(here::here("runtime_setup",
                  "2_plotting_functions.R"), local = T)

logger_file <- file.path(runtime_path, paste0("compiling_logger_outputs_",
                                              start_year,
                                              ".log"))
## define the file logger with log rotation enabled
log_appender(appender_file(logger_file, max_files = 1L,
                           max_lines = Inf, max_bytes = Inf))

log_threshold(TRACE)
log_threshold()

tic()  ## ** combining yearly ehe/ece boundaries sf  ----
list.files(daily_sp_outputs)
ehe_ece_boundaries <- fx_combine_rds(daily_sp_outputs, 
                                     paste0("idw_ehce_boundaries_sf_", start_year)) 
fx_toc(ehe_ece_boundaries, 1, time_period, cellsize)

glimpse(ehe_ece_boundaries)

saveRDS(ehe_ece_boundaries,
       file.path(yearly_dataset_path,
                 paste0(start_year,
                        "_ehe_ece_boundaries_",
                        cellsize,
                        ".rds")))

st_write(ehe_ece_boundaries,
         file.path(yearly_dataset_path,
                   paste0(start_year,
                          "_ehe_ece_vector_boundaries_",
                          cellsize,
                          ".gpkg")),
         delete_layer = TRUE)


tic()  ## ** combining yearly impacted area vars intensity sf  ----

list.files(daily_impacted_ehce_outputs)

impacted_geo_boundaries <- fx_combine_rds(daily_impacted_ehce_outputs, # only include the events intensity surfaces
                                          paste0("_geo_ehe_ece_", start_year))
fx_toc(impacted_geo_boundaries, 1, time_period, cellsize)

st_write(impacted_geo_boundaries,
         file.path(yearly_dataset_path,
                   paste0(start_year,
                          "_impacted_area_ehce_intensity_",
                          cellsize,
                          ".gpkg")),
         delete_layer = TRUE)

impacted_areas_vars_intensity <- fx_combine_rds(daily_impacted_stack_outputs, # all variables intensity measures
                                              paste0("_", start_year))
fx_toc(impacted_areas_vars_intensity, 1, time_period, cellsize)

st_write(impacted_areas_vars_intensity,
         file.path(yearly_dataset_path,
                   paste0(start_year,
                          "_impacted_area_variables_intensity_",
                          cellsize,
                          ".gpkg")),
         delete_layer = TRUE)

log_debug("yearly VECTOR data processing completed")

fx_reportFieldsPar(
  ehe_ece_boundaries,
  impacted_geo_boundaries,
  impacted_areas_vars_intensity,
  
  prefix = time_period,
  suffix = "vector",
  output_path = yearly_dataset_path)

rm(ehe_ece_boundaries)
rm(impacted_geo_boundaries)
rm(impacted_areas_vars_intensity)

dir.create(file.path(reports_path , paste0("viz_summary_plots_", start_year)))
plots_output_path <- file.path(reports_path, paste0("viz_summary_plots_", start_year))

tic()  ## ** combining yearly ehmi/ecmi idw rasters  ----

idw_grids <- fx_combine_rasters(daily_idw_rasters_outputs,  
                                paste0("idw_ehce_grid_", start_year),
                                yearly_dataset_path,
                                paste0(start_year,
                                       "_idw_ehce_grids_",
                                       cellsize),
                                start_year,
                                idw_var_name = "IDW",
                                plot_ranges = c(0, 1500, 100))

fx_toc(idw_grids, 1, time_period, cellsize)


tryCatch({
tic()  ## ** combining yearly ece unified rasters  ----
list.files(daily_unified_rasters_outputs)

ece_unified_grids <- fx_combine_rasters(daily_unified_rasters_outputs, 
                                        paste0("idw_ece_raster_unified_", start_year),
                                        yearly_dataset_path,
                                        paste0(start_year,
                                               "_ece_unified_grids_",
                                               cellsize),
                                        start_year,
                                        idw_var_name = "ECE",
                                        plot_ranges = c(0, 40, 10))
fx_toc(ece_unified_grids, 1, time_period, cellsize)

}, error = function(e){
  log_warn("ece compiling error")
  log_error(conditionMessage(e))
})

tryCatch({
tic()  ## ** combining yearly ehe unified rasters  ----
ehe_unified_grids <- fx_combine_rasters(daily_unified_rasters_outputs, 
                                        paste0("idw_ehe_raster_unified_", start_year),
                                        yearly_dataset_path,
                                        paste0(start_year,
                                               "_ehe_unified_grids_",
                                               cellsize),
                                        start_year,
                                        idw_var_name = "EHE",
                                        plot_ranges = c(0, 40, 10))

fx_toc(ehe_unified_grids, 1, time_period, cellsize)
}, error = function(e){
  log_warn("ehe compiling error")
  log_error(conditionMessage(e))
})


tryCatch({
  tic()  ## ** combining yearly ehce unified rasters  ----
  ehce_unified_grids <- fx_combine_rasters(daily_unified_rasters_outputs, 
                                          paste0("_raster_unified_", start_year),
                                          yearly_dataset_path,
                                          paste0(start_year,
                                                 "_ehce_unified_grids_",
                                                 cellsize),
                                          start_year,
                                          idw_var_name = "EHCE",
                                          plot_ranges = c(0, 60, 10))
  
  fx_toc(ehce_unified_grids, 1, time_period, cellsize)
}, error = function(e){
  log_warn("ehe compiling error")
  log_error(conditionMessage(e))
})

log_success("yearly RASTER data processing completed")

params_suffix <- time_period
fx_histboxplot(selected_period_geo_events_only, "month_name", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----
fx_histboxplot(selected_period_geo_events_only, "Climate_Region", plots_output_path, params_suffix) # // . ----[histoboxplot]---- . ----

rm(idw_grids)
rm(ehe_unified_grids)
rm(ece_unified_grids)

log_info(paste0("Data processing completed!"))

