# Latest update Jan 2024
library(terra)

# Create a function to impute missing values within a station
fx_impute_station <- function(qc_daily_summaries_int1_pass_completeness) {
  
  qc_daily_summaries_int2_maxgap_imputed <- qc_daily_summaries_int1_pass_completeness %>%
    # Impute missing values ----
  tic()
  # * 2 imputation settings for ordinary interpolation----
  imputeTS::na_interpolation(., maxgap = maxgap, option = "linear")
  fx_toc(qc_daily_summaries_int2_maxgap_imputed, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  tic()
  qc_daily_summaries_int3_kalman_imputed <- qc_daily_summaries_int2_maxgap_imputed %>%
    # * 3 imputation settings Kalman----
  imputeTS::na_kalman(., maxgap = maxgap + 2)
  fx_toc(qc_daily_summaries_int3_kalman_imputed, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  #qc_daily_summaries_int4_unlimited_kalman <- qc_daily_summaries_int2_maxgap_imputed %>%
  #imputeTS::na_kalman(.) # for testing purpose
  
  fx_reportFieldsPar(
    all_daily_summaries_inf2na,
    qc_daily_summaries_int1_pass_completeness,
    qc_daily_summaries_int2_maxgap_imputed,
    qc_daily_summaries_int3_kalman_imputed,
    #qc_daily_summaries_int4_unlimited_kalman,
    
    prefix = threshold_label,
    suffix = paste0("maxgap", maxgap),
    output_path = geo_output_path)
  
  qc_daily_summaries_imputed <- qc_daily_summaries_int3_kalman_imputed
  
  glimpse(qc_daily_summaries_imputed)
  #"all_daily_summaries_inf2na",
  #"qc_daily_summaries_int1_pass_completeness",
  #"qc_daily_summaries_int2_maxgap_imputed",
  #"qc_daily_summaries_int3_kalman_imputed"
  #qc_daily_summaries_int4_unlimited_kalman
  # sum(is.na(qc_daily_summaries_imputed$temperature_avg))
  # sum(is.infinite(qc_daily_summaries_imputed$temperature_dewpoint_avg))
  # hist(qc_daily_summaries_imputed$wind_speed_avg)
  #indexShow()
  
  ## Compute Heat Index ----
  tic()
  qc_daily_summaries_imputed_heat_index <- qc_daily_summaries_imputed %>%
    mutate(relative_humidity_avg = weathermetrics::dewpoint.to.humidity(t = temperature_avg,
                                                                        dp = temperature_dewpoint_avg,
                                                                        temperature.metric = "celsius")) %>%
    
    mutate(relative_humidity_avg_hum_lib = humidity::RH(t = temperature_avg,
                                                        Td = temperature_dewpoint_avg,
                                                        isK = F)) %>%
    
    # address observations for which the imputed dew point temperature was higher than the temperature
    # * 4 imputation settings Kalman/NoKalman----
  imputeTS::na_interpolation(., maxgap = maxgap + 2) %>% #imputeTS::na_kalman(.) %>%
    
    # https://github.com/caijun/humidity (2019)
    # unit is 
    mutate(vapure_pressure_avg = humidity::WVP1(Td = temperature_dewpoint_avg, 
                                                isK = F) * 0.1) %>%
    # convert hectopascal (hPa) to kilopascal (kPa) 
    
    # https://github.com/anacv/HeatStress
    mutate(apparent_temperature_heatstress_lib = HeatStress::apparentTemp(
      tas = temperature_avg,
      hurs = relative_humidity_avg,
      wind = wind_speed_avg)) %>%
    
    mutate(affective_temperature_heatstress_lib = HeatStress::effectiveTemp(
      tas = temperature_avg,
      hurs = relative_humidity_avg,
      wind = wind_speed_avg)) %>%
    
    # mutate(wbgt_heatstress_lib = round(HeatStress::wbgt.Bernard(
    #   tas = temperature_avg,
    #   dewp = temperature_dewpoint_avg,
    #   noNAs = TRUE,
    #   swap = FALSE), 1)) %>%
    
    mutate(heat_index_heatstress_lib = HeatStress::hi(
      tas = temperature_avg,
      hurs = relative_humidity_avg)) %>%
    
    # https://github.com/geanders/weathermetrics (2017)
    mutate(heat_index_weathermetrics_lib = weathermetrics::heat.index(
      t = temperature_avg,
      dp = temperature_dewpoint_avg,
      temperature.metric = "celsius")) %>%
    
    # Apparent Temperature Computation ----
  mutate(apparent_temperature = -2.7 + (1.04 * temperature_avg) +
           (2 * vapure_pressure_avg) - 
           (0.65 * wind_speed_avg)) %>%
    
    mutate(diff_avgat_vs_avgt = apparent_temperature - temperature_avg) %>% 
    mutate_if(is.double, ~ round(., 1))
  #mutate(diff_humidity = relative_humidity_avg_WMetrics - relative_humidity_avg)
  
  fx_toc(qc_daily_summaries_imputed_heat_index, 0, threshold_label, paste0(maxgap, "maxgap"))
  
  return(qc_daily_summaries_imputed_heat_index)
}


fx_census_areal_interpolation <- function(in_sf = in_sf)
{
  out_sf = st_interpolate_aw(census_db_sf %>%
                               dplyr::select(
                                 #where(is.numeric)
                                 starts_with("d_")), # select variables that marked by "_d" tag
                             in_sf,
                             extensive = TRUE,
                             crs = spatial_projection) %>%
    rename_with(~str_c(., "_sum"), !starts_with("geometry")) %>%
    mutate(YYYY_MM_DD = as.Date(format(selected_day), "%Y-%m-%d")) %>%
    mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
    mutate(area_hectare = round((computed_area / 10000), 1)) %>%
    mutate_if(is.double, ~ round(., 0))
  
  return(out_sf)
}


# f(x): take sf point and return sf voronoi with data
fx_VoronoiPolygons_fromSF <- function(points){
  if(!all(st_geometry_type(points) == "POINT")){
    stop("Input not  POINT geometries")
  }
  g = st_combine(st_geometry(points)) # make multipoint
  v = st_voronoi(g)
  v = st_collection_extract(v)
  w = v[unlist(st_intersects(points, v))]
  pv = st_set_geometry(points, w)
  return(pv)
}


fx_ehe_ece_idw <- function(points_df = station_points_SP, 
                           grid_sp = blank_grid,
                           idw_formula = "EHCMI_normalized ~ 1",
                           idp = idp,
                           nmax = nmax){
  # + + + Set the interpolation variable ----
  tic()
  interpolated_idw_SP <- gstat::idw(as.formula(idw_formula),
                                    points_df,
                                    newdata = blank_grid_SP, # base grid
                                    nmax = nmax,
                                    idp = idp)

  fx_toc(interpolated_idw_SP, 1, selected_day_label, "sf_grid")
  
  # interpolated_idw_stack_stars <- stars::st_as_stars(interpolated_idw_stack)
  # fx_toc(interpolated_idw_stack_stars, 1, selected_day_label, names(interpolated_idw_stack))
  # rm(interpolated_idw_stack)
  # rm(interpolated_idw_stack_stars) # NOT WORKING YET
  
  #rm(interpolated_idw_SP2Terra)
  
  
  #tryCatch({
  # Create the Raster object that is needed for computing cumulative & differential stats, not suitable for visualization
  tic()
  #### IWD 2 Raster ----
  idw_ehce_raster_bbox <- raster::raster(interpolated_idw_SP["var1.pred"])
  #idw_ehce_raster_bbox <- st_rasterize(interpolated_idw_SP["var1.pred"])
  
  names(idw_ehce_raster_bbox) <- selected_day
  
  idw_ehce_raster <- raster::mask(raster::crop(idw_ehce_raster_bbox,
                                               masking_layer), masking_layer)
  rm(idw_ehce_raster_bbox)
  idw_ehce_raster <- raster::trim(idw_ehce_raster)
  
  # Export 1 ----
  terra::writeRaster(idw_ehce_raster,
                     file.path(geo_output_path,
                               paste0("idw_ehce_grid_",
                                      selected_day)),
                     filetype = "raster",
                     overwrite=TRUE)
  
  fx_toc(idw_ehce_raster, 1, selected_day_label, core_id)
  
  tic()
  #### Raster 2 contours----
  # idw_ehce_contours <- NULL
  # idw_ehce_contours <- idw_ehce_contours[0,]
  # tryCatch({
  #   idw_ehce_contours <- rasterTocontours(idw_ehce_raster,
  #                                       nlevels = 7,
  #                                       maxpixels=1000000
  #   ) %>%  
  #     st_as_sf(.,
  #              crs = spatial_projection)
  # }, error = function(e){
  #   #  idw_ehce_contours <- idw_ehce_contours[0,]
  #   
  #   cat("ERROR :",conditionMessage(e), "\n")
  #   return(idw_ehce_contours)
  # })
  # fx_toc(idw_ehce_contours, 1, selected_day_label, core_id)
  #plot(idw_ehce_raster)
  #plot(idw_ehce_contours, add = T)
  
  # Export 2 ----
  # tic() 
  # idw_ehce_dt <- as.data.frame(idw_ehce_raster, xy=TRUE)
  # 
  # setDT(idw_ehce_dt)
  # 
  # # table_path <- file.path(tables_output_path,
  # #                         paste0("idw_ehce_dt_",
  # #                                selected_day,
  # #                                ".csv"))
  # #
  # # write.table(idw_ehce_dt, table_path,
  # #             sep = ",", row.names = F, col.names = T,
  # #             append = F) # write the large csv files
  # 
  # fx_toc(idw_ehce_dt, 0, selected_day_label, core_id)
  # rm(idw_ehce_dt) #### remove data.table object ---
  
  
  tic() # terra -----
  # Create the Raster object that is needed for visualization
  #### Raster 2 Terra ----
  idw_ehce_terra_raster <- terra::rast(idw_ehce_raster)
  crs(idw_ehce_terra_raster) <- paste0("epsg:", spatial_projection)
  # Crop raster to polygon extent
  idw_ehce_terra_raster <- crop(idw_ehce_terra_raster, extent(masking_layer))
  
  #idw_ehce_terra_raster[(idw_ehce_terra_raster < EHE_threshold) & (idw_ehce_terra_raster > ECE_threshold)] <- NA #clear up around the zero
  idw_ehce_terra_raster[(idw_ehce_terra_raster < 0.99) & (idw_ehce_terra_raster > -0.99)] <- NA #clear up around the zero
  #terra::project(idw_ehce_terra_raster, crs(spatial_projection))
  
  terra::writeRaster(idw_ehce_terra_raster,
                     file.path(daily_idw_rasters_outputs,
                               paste0("idw_ehce_grid_",
                                      selected_day, ".tif")),
                     filetype = "GTiff",
                     overwrite=TRUE)
  
  # writeCDF(idw_ehce_terra_raster, 
  #          file.path(idw_outputs, "netcdf.nc"),
  #          varname=paste0("layer_", selected_day), 
  #          overwrite=F)
  fx_toc(idw_ehce_terra_raster, 1, selected_day_label, core_id)
  rm(idw_ehce_raster) #### remove raster masked object ----
  
  # saveRDS(idw_ehce_terra_raster,
  #         file.path(daily_sp_outputs,
  #                   paste0("idw_ehce_terra_raster_",
  #                          selected_day,
  #                          ".rds")))
  
  tic()
  idw_ehce_terra_contours <- terra::as.polygons(idw_ehce_terra_raster)
  fx_toc(idw_ehce_terra_contours, 1, selected_day_label, core_id)
  rm(idw_ehce_terra_contours) #### remove terra_contours object ----
  
  
  # tic()
  # #### Raster 2 contours----
  # idw_ehce_contours_fromterra <- NULL
  # idw_ehce_contours_fromterra <- idw_ehce_contours_fromterra[0,]
  # tryCatch({
  #   idw_ehce_contours_fromterra <- rasterTocontours(idw_ehce_terra_raster,
  #                                       nlevels = 7,
  #                                       maxpixels=1000000
  #   ) %>%  
  #     st_as_sf(.,
  #              crs = spatial_projection)
  # }, error = function(e){
  #   #  idw_ehce_contours <- idw_ehce_contours[0,]
  #   
  #   cat("ERROR :",conditionMessage(e), "\n")
  #   return(idw_ehce_contours_fromterra)
  # })
  # fx_toc(idw_ehce_contours_fromterra, 1, selected_day_label, core_id)
  #plot(idw_ehce_raster)
  #plot(idw_ehce_contours, add = T)
  
  # terra2stars ++++ -----
  tic()
  
  idw_ehce_Stars_from_terra_raster <- stars::st_as_stars(idw_ehce_terra_raster, 
                                                         #flip(idw_ehce_terra_raster, "horizontal"),
                                                         crs = st_crs(spatial_projection))
  
  # this fixes stars dimensions display order, while makes a new issue for gstat::idw 
  #dimnames(idw_ehce_Stars_from_terra_raster) <- c("y", "x") 
  
  fx_toc(idw_ehce_Stars_from_terra_raster, 0, selected_day_label, core_id)
  
  rm(interpolated_idw_SP) #### remove sp object ----
  
  #### Raster 2 Events Boundaries ----
  tic()
  #### IDW 2 Terra Vector  ----
  idw_ehce_raster_unified <- idw_ehce_terra_raster
  #idw_ehce_raster_unified[(idw_ehce_raster_unified < EHE_threshold) & (idw_ehce_raster_unified > ECE_threshold)] <- NA
  # + + + Thresholds ----
  idw_ehce_raster_unified[(idw_ehce_raster_unified < 0)] <- -1 ## ECE_threshold
  idw_ehce_raster_unified[(idw_ehce_raster_unified > 0)] <- 1 ## EHE_threshold
  #plot(idw_ehce_raster_unified)
  #plot(masking_layer, add=T)
  # Assign codes +1/-1 to the event polygon boundaries to distinguish Cold and Heat Events
  
  # terra::writeVector(idw_ehce_boundaries_terrav,
  #                    file.path(geo_output_path,
  #                              paste0("idw_ehce_terra_vector_",
  #                                     selected_day)),
  #                    overwrite=TRUE) #works, creates shapefile
  
  # ToDo assign summary stats of the intepolated surface (underlaying raster)
  
  # Create two rasters one for EHE impacted area / one for ECE
  idw_ece_raster_unified <- idw_ehce_raster_unified
  idw_ece_raster_unified[(idw_ece_raster_unified > 0)] <- NA
  idw_ece_raster_unified <- abs(idw_ece_raster_unified)
  
  if (length(table(is.na(idw_ece_raster_unified[]))) == 2) {
    # saveRDS(idw_ece_raster_unified,
    #       file.path(daily_unified_rasters_rds_outputs,
    #                 paste0("idw_ece_raster_unified_",
    #                        selected_day,
    #                        ".rds")))
    
    terra::writeRaster(idw_ece_raster_unified,
                       file.path(daily_unified_rasters_outputs,
                                 paste0("idw_ece_raster_unified_",
                                        selected_day, ".tif")),
                       filetype = "GTiff",
                       overwrite=TRUE)
  }
  
  idw_ehe_raster_unified <- idw_ehce_raster_unified
  idw_ehe_raster_unified[(idw_ehe_raster_unified < 0)] <- NA
  
  if (length(table(is.na(idw_ehe_raster_unified[]))) == 2) {
    # saveRDS(idw_ehe_raster_unified,
    #         file.path(daily_unified_rasters_rds_outputs,
    #                   paste0("idw_ehe_raster_unified_",
    #                          selected_day,
    #                          ".rds")))
    
    terra::writeRaster(idw_ehe_raster_unified,
                       file.path(daily_unified_rasters_outputs,
                                 paste0("idw_ehe_raster_unified_",
                                        selected_day, ".tif")),
                       filetype = "GTiff",
                       overwrite=TRUE)
  } 
  
  
  fx_toc(idw_ehce_raster_unified, 0, selected_day_label, core_id)
  #ls(idw_ehce_boundaries_terrav)
  # fx_reportFieldsPar(
  #   idw_ehce_boundaries_terrav,
  #   
  #   prefix = cellsize,
  #   suffix = st_label,
  #   output_path = meta_output_path)
  tic()
  idw_ehce_raster_unified <- aggregate(idw_ehce_raster_unified)
  idw_ehce_boundaries_terrav <- as.polygons(idw_ehce_raster_unified) 
  fx_toc(idw_ehce_boundaries_terrav, 1, selected_day_label, core_id)
  
  
  tic() # sf -----
  idw_ehce_boundaries_sf <- idw_ehce_boundaries_terrav %>% 
    st_as_sf() %>% st_transform(spatial_projection)  %>% st_cast("POLYGON") %>% #  MULTIPOLYGON ----
    mutate(event_date = as.Date(selected_day, "%a, %d %b %Y")) %>%
    mutate(computed_event_area = round(as.numeric(st_area(.)), 0)) %>%
    mutate(area_hectare = round((computed_event_area / 10000), 1)) %>% 
    st_zm(., drop=TRUE, what = "ZM") %>% 
    st_buffer(.,
              dist = 0,
              nQuadSegs = 30,
              endCapStyle = "ROUND",
              joinStyle = "ROUND",
              mitreLimit = 1,
              singleSide = FALSE) 
  
  names(idw_ehce_boundaries_sf)[1] <- "event_type_code"
  
  idw_ehce_boundaries_sf <- idw_ehce_boundaries_sf %>% 
    filter(event_type_code != 0) %>% 
    mutate(event_type =
             case_when(event_type_code == 1 ~ "Extreme Heat Event", 
                       event_type_code == -1 ~ "Extreme Cold Event",
                       TRUE ~ "NA")) 
  
  fx_toc(idw_ehce_boundaries_sf, 0, selected_day_label, core_id)
  saveRDS(idw_ehce_boundaries_sf,
          file.path(daily_sp_outputs,
                    paste0("idw_ehce_boundaries_sf_",
                           selected_day,
                           ".rds")))
  
  #glimpse(idw_ehce_boundaries_terrav)
  
  #plot(idw_ehce_boundaries_terrav)
  #plot(idw_ehce_boundaries_sf[1])
  
  # tic()
  # #### IDW 2 Stars ----
  # idw_ehce_boundaries_terrav_stars <- stars::st_as_stars(idw_ehce_boundaries_terrav)
  # fx_toc(idw_ehce_boundaries_terrav_stars, 1, selected_day_label, core_id)
  # rm(idw_ehce_boundaries_terrav)
  # rm(idw_ehce_boundaries_terrav_stars) #### remove stars object ----
  
  tic()
  idw_ehce_boundaries_sf_stars <- stars::st_as_stars(idw_ehce_boundaries_sf)
  fx_toc(idw_ehce_boundaries_sf_stars, 1, selected_day_label, core_id)
  #plot(idw_ehce_boundaries_sf_stars)
  #glimpse(idw_ehce_boundaries_sf_stars)
  #rm(idw_ehce_boundaries_sf)
  
  rm(idw_ehce_boundaries_sf_stars) #### remove sf object ----
  #rm(idw_ehce_raster_unified) #### remove raster unified object ----
  #rm(idw_ehce_terra_raster) #### remove terra raster object ----
  rm(idw_ehce_contours)
  
  outputs_object <- list(idw_ehce_boundaries_sf,
                         idw_ehce_terra_raster,
                         idw_ehce_Stars_from_terra_raster,
                         idw_ehce_raster_unified
                         #interpolated_idw_DM_AT, 
                         #interpolated_idw_EHCF
  )
  
  return(outputs_object)
}

fx_vars_idw <- function(idw_var_name = "DM_AT",
                        idw_formula = "DM_AT ~ 1",
                        points_df = station_points_SP,  
                        trimmed_grid_stars = trimmed_grid_stars, # get stars object as input
                        idp = idp,
                        nmax = nmax){
  
  tic()
  # // Drop NA ??????? //----
  station_points_SP_NN <- station_points_SP[!is.na(station_points_SP[[idw_var_name]]), ]
  #print(dim(station_points_SP_NN))
  
  var_interpolated_idw_stars <- gstat::idw(as.formula(idw_formula),
                                           station_points_SP_NN,
                                           newdata = as(trimmed_grid_stars, "Spatial"), # sp grid from trimmed stars
                                           nmax = nmax,
                                           idp = idp)
  
  var_interpolated_idw_stars <- raster::raster(var_interpolated_idw_stars["var1.pred"])
  
  names(var_interpolated_idw_stars) <- idw_var_name
  
  var_interpolated_idw_terrast <- terra::rast(var_interpolated_idw_stars[[1]])
  names(var_interpolated_idw_terrast) <- idw_var_name
  crs(var_interpolated_idw_terrast) <- paste0("epsg:", spatial_projection)
  fx_toc(var_interpolated_idw_terrast, 0, selected_day_label, idw_var_name)
  rm(var_interpolated_idw_stars)
  
  return(var_interpolated_idw_terrast)
}


fx_combine_rasters <- function(path, pattern, 
                               grid_output_path = geo_output_path,
                               output_filename = "yearly_var_dataset",
                               year_label = 2008,
                               idw_var_name = "EHE / ECE / IDW",
                               plot_ranges = plot_ranges) {
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and create a list of rasters
  raster_list <- lapply(files, rast)
  
  dates <- substr(files, nchar(files) - 13, nchar(files) - 4)
  
  # Combine the list of rasters into a SpatRaster object
  combined_rasters <- do.call(c, raster_list)
  
  names(combined_rasters) <- as.list(dates)
  
  cumulative_grid_terra <- app(combined_rasters, sum, na.rm = TRUE)
  names(cumulative_grid_terra) <- year_label
  
  terra::writeRaster(combined_rasters,
                     file.path(grid_output_path, 
                               paste0(output_filename,
                                      "_stack",
                                      ".tif")),
                     filetype = "GTiff",
                     overwrite=TRUE)
  
  terra::writeRaster(cumulative_grid_terra,
                     file.path(grid_output_path, 
                               paste0(output_filename,
                                      "_cumulative",
                                      ".tif")),
                     filetype = "GTiff",
                     overwrite=TRUE)
  
  cumulative_event_coverage <- 100 * round(
    (length(cumulative_grid_terra[!is.na(cumulative_grid_terra)]) / total_grids), 3) # // coverage ----
  
  fx_cumulative_impact_plot(plots_output_path,
                            cumulative_grid_terra, 
                            map_divisions = map_divisions,
                            year = start_year,
                            output_filename = output_filename,
                            cumulative_event_coverage = cumulative_event_coverage,
                            idw_var_name = idw_var_name,
                            plot_ranges = plot_ranges)
  
  ## <<< viz cumulative raster map ----
  plot_file_name <- file.path(plots_output_path, 
                              paste0(output_filename,
                                     "_cumulative_impacts_",
                                     "00_plot_",
                                     cumulative_event_coverage,
                                     "_percent.jpg"))
  
  # jpeg(plot_file_name, width = 1200, height = 800)
  # print(plot(idw_blank_raster, col = "gray80"))
  # print(plot(cumulative_grid_terra, add = T, 
  #            main = cumulative_event_coverage, sub = start_year))
  # dev.off()
  
  plot_file_name <- file.path(plots_output_path, 
                              paste0(output_filename,
                                     "_cumulative_impacts_",
                                     "01_spplot_",
                                     cumulative_event_coverage,
                                     "_percent.jpg"))
  
  #jpeg(plot_file_name, width = 1800, height = 1200)
  #print(spplot(cumulative_grid_terra, main = paste0(cumulative_event_coverage, " ", start_year)))
  #dev.off()
  
  plot_file_name <- file.path(plots_output_path, 
                              paste0(output_filename,
                                     "_cumulative_impacts_",
                                     "02_teneka_",
                                     cumulative_event_coverage,
                                     "_percent.jpg"))
  
  # jpeg(plot_file_name, width = 1800, height = 1200)
  # 
  # lower_limit <- minmax(cumulative_grid_terra)[1,1]
  # upper_limit <- minmax(cumulative_grid_terra)[2,1]
  # 
  # print(tanaka::tanaka(cumulative_grid_terra,
  #                      col = hcl.colors(n = 12, palette = "Inferno"),
  #                      breaks = seq(lower_limit,
  #                                   upper_limit, 50),
  #                      mask = map_divisions,
  #                      legend.pos = "topright"
  #                      #legend.title = "Number of ECE/EHE\n(days)"
  #                      ))
  # dev.off()
  
  fx_reportRasterMetadata(
    cumulative_grid_terra,
    
    prefix = "",
    suffix = "study_period_yearly",
    output_path = yearly_dataset_path,
    param_1 = idw_var_name,
    param_2 = year_label,
    param_3 = cumulative_event_coverage)
  
  outputs_object <- list(combined_rasters, 
                         cumulative_grid_terra,
                         dates)
  return(outputs_object)
}

fx_impacted_geo <- function(admin_geo, ehce_boundaries_geo, 
                            interpolated_raster,
                            interpolated_raster_var_name,
                            admin_geo_label = "zones",
                            model_params = model_params){
  #admin_geo <- counties_geo 
  #ehce_boundaries_geo <- idw_ehce_boundaries_sf
  
  admin_geo_impacted_points <- st_intersection(admin_geo["GEOID"],
                                               ehce_boundaries_geo) %>% 
    st_transform(spatial_projection) %>% st_cast("POLYGON") %>% st_zm(., drop=TRUE, what = "ZM") %>% 
    mutate(impacted_area = round(as.numeric(st_area(.)), 0)) %>%  st_centroid() %>% 
    st_cast("POINT")
  
  admin_geo_interim_polygons <- st_join(admin_geo[c("GEOID")] %>% 
                                          mutate(total_area = round(as.numeric(st_area(.)), 0)),
                                        
                                        admin_geo_impacted_points[c("event_date",
                                                                    "event_type",
                                                                    "impacted_area")],
                                        join = st_covers, largest = TRUE) %>% # + + + Intersection ----
  # st_covers
  #filter(!is.na(impacted_area)) %>% 
  filter(impacted_area > 0) %>% 
    st_as_sf() %>% as_tibble() %>% st_as_sf() %>% 
    mutate(impacted_to_total_ratio = round((impacted_area / total_area), 3)) %>% 
    #mutate(event_date = as.Date(selected_day, "%a, %d %b %Y")) %>% 
    mutate(impacted_area_hectare = round((impacted_area / 10000), 1)) %>% 
    mutate(geographic_scale = admin_geo_label) %>% 
    dplyr::select(
      !starts_with("A") &
        !ends_with("LSAD") &
        !ends_with ("FP")) 
  
  cells_summary_fun <- function(x) { #var intensity summary * * * *-----
    c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),  
      sd = sd(x, na.rm = TRUE),
      numcells = length(x)
    )
  }
  
  # Crop raster to polygon extent
  #raster_crop <- crop(interpolated_raster, extent(admin_geo_interim_polygons))
  
  # Mask raster with polygons
  #raster_masked <- mask(raster_crop, admin_geo_interim_polygons)
  
  event_geo_intensity_summary <- 
    terra::extract(
      interpolated_raster,
      admin_geo_interim_polygons,
      fun = cells_summary_fun,
      touches = TRUE) ## https://rdrr.io/cran/terra/man/extract.html
  
  #na.rm = TRUE,
  colnames(event_geo_intensity_summary) <- c("ID", 
                                             "min_intensity", "max_intensity",
                                             "avg_intensity", "median_intensity",  
                                             "sd_intensity",
                                             "numcells")
  
  admin_geo_compiled_polygons <- admin_geo_interim_polygons %>% 
    mutate(ID := seq_len(nrow(.))) %>% 
    left_join(., event_geo_intensity_summary, by = "ID") %>% 
    mutate(across(contains("intensity"), round, 1)) %>% 
    #filter(!is.na(sd_intensity)) %>%  # remove the records of single overlapping cells
    #filter(!avg_intensity == 0) %>%
    #left_join(., event_n_intensity, by = "ID") %>% 
    #rename_with(~"ncells_intensity", .cols = last_col()) %>% 
    mutate(idw_variable = interpolated_raster_var_name) %>% 
    mutate(model_params = model_params) %>% 
    dplyr::select(!ID)
  
  admin_geo_compiled_points <- admin_geo_compiled_polygons %>% st_centroid()
  
  admin_geo_compiled_table <- admin_geo_compiled_polygons %>% st_drop_geometry()
  
  rolling_table_path <- file.path(reports_path,
                                  paste0(admin_geo_label,
                                         "_geo_compiled_table_yearly_",
                                         time_period,
                                         ".csv"))
  # write.table(admin_geo_compiled_table,
  #             rolling_table_path,
  #             sep = ",", row.names = F, 
  #             col.names = !file.exists(rolling_table_path),
  #             append = T) 
  
  table_path <- file.path(tables_output_path,
                          paste0(admin_geo_label, "_",
                                 interpolated_raster_var_name,
                                 "_geo_compiled_table_",
                                 selected_day,
                                 ".csv"))
  
  write.table(admin_geo_compiled_table, 
              
              table_path,
              sep = ",", row.names = F, col.names = T,
              append = F)
  
  outputs_object <- list(admin_geo_compiled_polygons,
                         admin_geo_compiled_points,
                         admin_geo_compiled_table,
                         
                         event_geo_intensity_summary,
                         admin_geo_impacted_points,
                         admin_geo_interim_polygons)
  
  return(outputs_object)
}


fx_impacted_geo_multi <- function(admin_geo, ehce_boundaries_geo, 
                                  interpolated_rasters,
                                  interpolated_raster_var_names,
                                  admin_geo_label = "zones",
                                  model_params = model_params) {
  admin_geo_impacted_points <- st_intersection(admin_geo["GEOID"],
                                               ehce_boundaries_geo) %>% 
    st_transform(spatial_projection) %>% st_cast("POLYGON") %>% st_zm(., drop=TRUE, what = "ZM") %>% 
    mutate(impacted_area = round(as.numeric(st_area(.)), 0)) %>%  st_centroid() %>% 
    st_cast("POINT")
  
  admin_geo_interim_polygons <- st_join(admin_geo[c("GEOID")] %>% 
                                          mutate(total_area = round(as.numeric(st_area(.)), 0)),
                                        
                                        admin_geo_impacted_points[c("event_date",
                                                                    "event_type",
                                                                    "impacted_area")],
                                        join = st_covers, largest = TRUE) %>% # + + + Intersection ----
  
  filter(impacted_area > 0) %>% 
    st_as_sf() %>% as_tibble() %>% st_as_sf() %>% 
    mutate(impacted_to_total_ratio = round((impacted_area / total_area), 3)) %>% 
    #mutate(event_date = as.Date(selected_day, "%a, %d %b %Y")) %>% 
    mutate(impacted_area_hectare = round((impacted_area / 10000), 1)) %>% 
    mutate(geographic_scale = admin_geo_label) %>% 
    dplyr::select(
      !starts_with("A") &
        !ends_with("LSAD") &
        !ends_with ("FP")) 
  
  cells_summary_fun <- function(x) { #var intensity summary * * * *-----
    c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),  
      sd = sd(x, na.rm = TRUE),
      numcells = length(x)
    )
  }
  
  results_list <- map2(interpolated_rasters, 
                       interpolated_raster_var_names, 
                       function(interpolated_raster, interpolated_raster_var_name) {
                         
                         event_geo_intensity_summary <- 
                           terra::extract(
                             interpolated_raster,
                             admin_geo_interim_polygons,
                             fun = cells_summary_fun,
                             touches = TRUE) ## https://rdrr.io/cran/terra/man/extract.html
                         
                         #na.rm = TRUE,
                         colnames(event_geo_intensity_summary) <- c("ID", 
                                                                    "min_intensity", "max_intensity",
                                                                    "avg_intensity", "median_intensity",
                                                                    "sd_intensity",
                                                                    "numcells")
                         
                         admin_geo_compiled_polygons <- admin_geo_interim_polygons %>% 
                           mutate(ID := seq_len(nrow(.))) %>% 
                           left_join(., event_geo_intensity_summary, by = "ID") %>% 
                           mutate(across(contains("intensity"), round, 1)) %>% 
                           #filter(!is.na(sd_intensity)) %>%  # remove the records of single overlapping cells
                           #filter(!avg_intensity == 0) %>%
                           #left_join(., event_n_intensity, by = "ID") %>% 
                           #rename_with(~"ncells_intensity", .cols = last_col()) %>% 
                           mutate(idw_variable = interpolated_raster_var_name) %>% 
                           mutate(model_params = model_params) %>% 
                           dplyr::select(!ID)
                         
                         return(admin_geo_compiled_polygons)
                       })
  
  admin_geo_compiled_polygons <- bind_rows(results_list) %>% st_as_sf()
  
  admin_geo_compiled_points <- admin_geo_compiled_polygons %>% st_centroid()
  
  admin_geo_compiled_table <- admin_geo_compiled_polygons %>% st_drop_geometry()
  
  table_path <- file.path(tables_output_path,
                          paste0(admin_geo_label, "_",
                                 #interpolated_raster_var_name,
                                 "_geo_stacked_vars_table_",
                                 selected_day,
                                 ".csv"))
  
  write.table(admin_geo_compiled_table, 
              
              table_path,
              sep = ",", row.names = F, col.names = T,
              append = F)
  
  outputs_object <- list(admin_geo_compiled_polygons,
                         admin_geo_compiled_points,
                         admin_geo_compiled_table)
  
  return(outputs_object)
}



fx_combine_yearly_rds <- function(runtime_results_path = runtime_results_path,
                                  admin_geo_label = "Counties",
                                  admin_geo_sf = admin_geo_sf,
                                  col_names = col_names,
                                  period = 3){
  gc()
  
  # admin_geo_sf_lite <- admin_geo_sf[, colnames(admin_geo_sf) %in% c("GEOID", "STUSPS", "Climate_Region")] %>% 
  #                      st_drop_geometry()
  admin_geo_sf_selected <- admin_geo_sf[, colnames(admin_geo_sf) %in% col_names]
  
  
  #admin_geo_sf <- states_geo #%>% st_buffer(.,
  # dist =  0,
  # endCapStyle = "ROUND",
  # joinStyle = "ROUND",
  # mitreLimit = 1,
  # singleSide = FALSE) %>% st_cast(., "MULTIPOLYGON")
  admin_geo_label = paste0(admin_geo_label, "_impacted_area_variables_stack_")
  compiled_admin_geo_ehe_ece <- fx_combine_rds(runtime_results_path, admin_geo_label)
  #glimpse(compiled_admin_geo_ehe_ece)
  
  compiled_admin_geo_ehe_ece <- compiled_admin_geo_ehe_ece %>% 
    #filter(idw_variable == idw_variable) %>% 
    filter(impacted_to_total_ratio <= 1) %>% 
    filter(impacted_to_total_ratio >= .001) %>% 
    st_drop_geometry() %>% 
    
    #filter(STATE_NAME %in% geog) %>% 
    dplyr::select(  # columns ----
                    #!starts_with("ST") &
                    #!ends_with("NAME") &
                    #!ends_with("intensity") &
                    #!ends_with("cells") &
                    #!ends_with("geographic_scale") &
                    #!ends_with("variable") &
                      !ends_with("NS")) %>% 
    mutate(
      year_numerical = lubridate::year(event_date),
      month_numerical = lubridate::month(format(event_date, "%Y-%m-%d")),
      month_name = as.character(lubridate::month(format(event_date, "%Y-%m-%d"), label = TRUE)),
      day_numerical = yday(event_date),
      #idw_variable = idw_variable,
      model_params = as.factor(model_params)) %>%
    #mutate(year_factor = as.factor(year_numerical)) %>%
    #mutate(month_factor = as.factor(month_name)) %>%    #na.omit(.) %>% 
    #filter(year_numerical >= period) %>% 
    right_join(admin_geo_sf_selected %>% st_drop_geometry(),
               .,
               by="GEOID") %>%
    as_tibble() 
  
  log_warn("reading events catalog data completed")
  setDT(compiled_admin_geo_ehe_ece)
  
  ls(compiled_admin_geo_ehe_ece)
  # compute summaries ----
  events_by_year <- compiled_admin_geo_ehe_ece %>% 
    group_by(GEOID, year_numerical, idw_variable) %>% 
    summarize(total_event_days = n_distinct(event_date),
              max_impacted_area_hectare = round(max(impacted_area_hectare,na.rm = TRUE), 1),
              average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
              sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
              .groups = "drop") %>% 
    mutate(model_params = as.factor(model_params))
  
  events_by_type_by_year <- compiled_admin_geo_ehe_ece %>% 
    group_by(GEOID, event_type, year_numerical, idw_variable) %>% 
    summarize(total_event_days = n_distinct(event_date),
              max_impacted_area_hectare = round(max(impacted_area_hectare,na.rm = TRUE), 1),
              average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
              sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
              .groups = "drop") %>% 
    mutate(model_params = as.factor(model_params)) %>% 
    filter(idw_variable == "ehcmi_normalized")
  
  
  # # Define a function that takes a data frame and an interval size as arguments
  # multiyear_aggregation <- function(df, interval) {
  #   # Create a new variable that indicates the interval
  #   df <- df %>%
  #     mutate(!!paste0("year_period_", interval) := cut(year_numerical, 
  #                                                   breaks = seq(min(year_numerical), max(year_numerical) + 1, 
  #                                                                by = interval), 
  #                                                   right = FALSE)) %>% 
  #     group_by(GEOID, NAME, event_type, !!paste0("year_period_", interval)) %>% 
  #     summarize(total_event_days = n_distinct(event_date),
  #               average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
  #               sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
  #               .groups = "drop") %>% 
  #     mutate(model_params = as.factor(model_params))
  #   # Return the modified data frame
  #   return(df)
  # }
  # 
  # events_by_type_by_year_3 <- multiyear_aggregation(compiled_admin_geo_ehe_ece, 3L)
  
  # intervals <- c(3, 5)
  # multi_year summaries <- map(intervals, multiyear_aggregation, df = compiled_admin_geo_ehe_ece)
  
  events_by_type_by_year_3 <- compiled_admin_geo_ehe_ece %>%
    mutate(year_period_3 = cut(year_numerical,
                               breaks = seq(min(year_numerical), max(year_numerical) + 1, by = 3), right = F)) %>%
    group_by(GEOID, event_type, year_period_3, idw_variable) %>% 
    summarize(total_event_days = n_distinct(event_date),
              max_impacted_area_hectare = round(max(impacted_area_hectare,na.rm = TRUE), 1),
              average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
              sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
              .groups = "drop") %>%
    mutate(model_params = as.factor(model_params))
  
  events_by_type_by_year_5 <- compiled_admin_geo_ehe_ece %>% 
    mutate(year_period_5 = cut(year_numerical, 
                               breaks = seq(min(year_numerical), max(year_numerical) + 1, by = 5), right = F)) %>% 
    group_by(GEOID, event_type, year_period_5, idw_variable) %>% 
    summarize(total_event_days = n_distinct(event_date),
              max_impacted_area_hectare = round(max(impacted_area_hectare,na.rm = TRUE), 1),
              average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
              sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
              .groups = "drop") %>% 
    mutate(model_params = as.factor(model_params))
  
  
  events_by_type <- compiled_admin_geo_ehe_ece %>% 
    group_by(GEOID, event_type, idw_variable) %>% 
    summarize(total_event_days = n_distinct(event_date),
              max_impacted_area_hectare = round(max(impacted_area_hectare,na.rm = TRUE), 1),
              average_impacted_area_hectare = round(mean(impacted_area_hectare,na.rm = TRUE), 1),
              sd_impacted_area_hectare = round(sd(impacted_area_hectare ,na.rm = TRUE), 1),
              .groups = "drop") %>% 
    mutate(model_params = as.factor(model_params))
  
  log_warn("computing summazries completed")
  # export non-spatial layers ----
  fx_saveCSVPar(
    events_by_year,
    events_by_type,
    events_by_type_by_year,
    events_by_type_by_year_3,
    events_by_type_by_year_5,
    
    prefix = admin_geo_label,
    suffix = time_period,
    output_path = tables_output_path)
  
  fx_reportFieldsPar(
    events_by_year,
    events_by_type,
    events_by_type_by_year,
    events_by_type_by_year_3,
    events_by_type_by_year_5,
    compiled_admin_geo_ehe_ece,
    
    prefix = admin_geo_label,
    suffix = time_period,
    output_path = meta_output_path)
  
  # join the summary data with sf layer ----
  events_by_year_sf <- events_by_year %>%
    merge(admin_geo_sf["GEOID"],
          .,
          by.x="GEOID",
          by.y="GEOID",
          #all.x = TRUE,
          all.y = TRUE,
          suffix = c("","_sp"), duplicateGeoms = T)
  
  events_by_type_sf <- events_by_type %>% 
    merge(admin_geo_sf["GEOID"],
          .,
          #by.x="GEOID",
          by.y="GEOID",
          all.x = TRUE,
          all.y = TRUE,
          suffix = c("","_sp"), duplicateGeoms = T) %>% 
    drop_na(event_type)
  
  events_by_type_by_year_sf <- events_by_type_by_year %>% 
    merge(admin_geo_sf["GEOID"],
          .,
          by.x="GEOID",
          by.y="GEOID",
          #all.x = TRUE,
          all.y = TRUE,
          suffix = c("","_sp"), duplicateGeoms = T) %>% 
    drop_na(event_type)
  
  events_by_type_by_year_3_sf <- events_by_type_by_year_3 %>% 
    right_join(admin_geo_sf["GEOID"],
               .,
               by="GEOID",
               #all.x = TRUE,
               # all.y = TRUE
    ) %>% 
    drop_na(event_type)
  
  events_by_type_by_year_5_sf <- events_by_type_by_year_5 %>% 
    right_join(admin_geo_sf["GEOID"],
               .,
               by="GEOID",
               #all.x = TRUE,
               # all.y = TRUE
    ) %>% 
    drop_na(event_type)
  
  compiled_admin_geo_ehe_ece_sf <- compiled_admin_geo_ehe_ece %>% 
     right_join(admin_geo_sf["GEOID"],
    #right_join(admin_geo_sf_selected,
               .,
               by="GEOID") %>% 
    drop_na(event_type) %>% st_as_sf()
  log_warn("events catalog created")
  
  # export spatial layers ----
  fx_saveRObjects(
    events_by_year_sf,
    events_by_type_sf,
    events_by_type_by_year_sf,
    events_by_type_by_year_3_sf,
    events_by_type_by_year_5_sf,
    compiled_admin_geo_ehe_ece_sf,
    
    prefix = admin_geo_label,
    suffix = time_period,
    output_path = rds_output_path)
  
  
  fx_reportFieldsPar(
    events_by_year_sf,
    events_by_type_sf,
    events_by_type_by_year_sf,
    events_by_type_by_year_3_sf,
    events_by_type_by_year_5_sf,
    compiled_admin_geo_ehe_ece_sf,
    
    prefix = admin_geo_label,
    suffix = time_period,
    output_path = meta_output_path)
  
  # saveRDS(compiled_admin_geo_ehe_ece,
  #         file.path(rds_output_path,
  #                   paste0("us_geo_ehe_ece_",
  #                          admin_geo_label,
  #                          ".rds")))
  
  table_path <- file.path(tables_output_path,
                          paste0(
                            "us_geo_ehe_ece_",
                            admin_geo_label,
                            ".csv"))
  
  # write.table(compiled_admin_geo_ehe_ece %>% st_drop_geometry(), 
  #             table_path,
  #             sep = ",", row.names = F, col.names = T,
  #             append = F,
  #             fileEncoding = "UTF-16LE")
  st_write(compiled_admin_geo_ehe_ece_sf,
           file.path(geo_output_path,
                     paste0("contiguous_us_",
                            admin_geo_label,
                            "_events_catalog_", time_period,
                            ".gpkg")),
           delete_layer = TRUE)
  
  # st_write(compiled_admin_geo_ehe_ece_sf,
  #          file.path(geo_output_path,
  #                    paste0("contiguous_us_",
  #                           admin_geo_label,
  #                           "_events_catalog_2008_2022",
  #                           ".geojson")),
  #          delete_layer = TRUE)
  
  outputs_object <- list(compiled_admin_geo_ehe_ece, 
                         compiled_admin_geo_ehe_ece_sf,
                         events_by_type,
                         events_by_type_sf,
                         events_by_type_by_year_sf,
                         events_by_type_by_year_3,
                         events_by_type_by_year_5,
                         events_by_year_sf
  )
  return(outputs_object)
}


fx_reportRasterMetadata <- function(...,
                                    output_path = tables_output_path,
                                    prefix = "00",
                                    suffix = "_metadata",
                                    param_1 = "model_params",
                                    param_2 = "model_params",
                                    param_3 = "model_params"
) {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  
  report <- map(1:length(object_names), function(i) {
    raster_layer <- objects[i][[1]]
    df_name <- object_names[i]
    
    raster_dim <- dim(raster_layer)
    ndim <- length(dim(raster_layer))

      nr <- nrow(raster_layer)
      nc <- ncol(raster_layer)
      ty <- typeof(raster_layer)
      cl <- class(raster_layer)
      msize <- format(object.size(raster_layer), units = "auto")
    
    if (inherits(raster_layer, "RasterLayer") || 
        inherits(raster_layer, "RasterStack") || 
        inherits(raster_layer, "RasterBrick")) {
      # For raster objects (raster package)
      number_of_layers <- nlayers(raster_layer)
      bbox <- extent(raster_layer)
      grid_cell_size <- res(raster_layer)
      
    } else if (inherits(raster_layer, "SpatRaster")) {
      # For terra objects (terra package)
      number_of_layers <- nlyr(raster_layer)
      crs <- st_crs(raster_layer)$input
      epsg <- st_crs(raster_layer)$epsg
      cell_size <- res(raster_layer)
      num_cells <- ncell(raster_layer)
      min_value <- minmax(raster_layer)[1,1]
      max_value <- minmax(raster_layer)[2,1]

      #prj <- proj4string(x)
      #st <- str(x)
      #sm <- summary(x)
      #methods <- methods(class = cl)
      #installed_methods <- apropos(cl)
      
          #"\n PRJ: ", prj
          #"\n CRS: ", crs
          #"\n Methods by class :", methods,
          #"\n Methods by keyword :", installed_methods
          #BBox = paste(c(bbox@xmin, bbox@ymin, bbox@xmax, bbox@ymax), collapse = ", "),)
      
    } else {
      stop("Unsupported raster type: ", class(raster_layer))
    }
    
    raster_report <- data.frame(
      Name = df_name,
      Param_1 = param_1,
      Param_2 = param_2,
      Param_3 = param_3,
      min_value = round(min_value, 3),
      max_value = round(max_value, 3),
      Number_of_Dimensions = ndim,
      Dimension = paste(raster_dim, collapse = " x "),
      Rows = nr,
      Columns = nc,
      Number_of_Layers = number_of_layers,
      Cell_size <- paste(cell_size, collapse = " x "),
      Number_of_cells <- num_cells,
      Size_in_memory = msize,
      EPSG = epsg,
      CRS = crs,
      Class= cl,
      Type = ty
      )
      
    print(raster_report)
  })
  
  combined_report <- do.call(rbind, report)
  
  table_path <- file.path(reports_path, 
                          paste0("gridded_raseters_report_", suffix, ".csv"))

  write.table(combined_report, table_path,
              sep = ",", 
              row.names = FALSE,
              col.names = !file.exists(table_path),
              append = T)
}



fx_impute_station_data <- function(data, date_col, station_id_col) {
  # Create a list of data frames, one for each station
  station_data_list <- split(data, data[[station_id_col]])
  
  # Define a function to impute missing values for a single data frame
  impute_station <- function(station_data) {
    # Sort the data by date to ensure chronological order
    station_data <- station_data[order(station_data[[date_col]]), ]
    
    # Apply Kalman imputation to numeric columns
    numeric_columns <- sapply(station_data, is.numeric)
    station_data[, numeric_columns] <- lapply(station_data[, numeric_columns], imputeTS::na_kalman(., maxgap = maxgap + 2))
    
    return(station_data)
  }
  
  # Use purrr's map to apply the imputation function to each data frame in the list
  imputed_data_list <- map(station_data_list, impute_station)
  
  # Combine the imputed data frames into a single data frame
  imputed_data <- do.call(rbind, imputed_data_list)
  
  return(imputed_data)
}


fx_reportRasterMetadata2 <- function(..., output_path = "tables_output_path", prefix = "00", suffix = "_metadata", param_1 = "model_params", param_2 = "model_params", param_3 = "model_params") {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  
  # Helper function to calculate bootstrap confidence intervals
  calculate_bootstrap_intervals <- function(raster_values, num_replicates = 1000, confidence_level = 0.95) {
    boot_stats <- replicate(num_replicates, {
      sample_indices <- sample(seq_along(raster_values), replace = TRUE)
      sample_values <- raster_values[sample_indices]
      c(min = min(sample_values, na.rm = TRUE),
        max = max(sample_values, na.rm = TRUE),
        mean = mean(sample_values, na.rm = TRUE))
    })
    
    alpha <- (1 - confidence_level) / 2
    c(min_lower = quantile(boot_stats[1,], alpha),
      min_upper = quantile(boot_stats[1,], 1 - alpha),
      max_lower = quantile(boot_stats[2,], alpha),
      max_upper = quantile(boot_stats[2,], 1 - alpha),
      mean_lower = quantile(boot_stats[3,], alpha),
      mean_upper = quantile(boot_stats[3,], 1 - alpha))
  }
  
  report <- lapply(1:length(objects), function(i) {
    raster_layer <- objects[[i]]
    df_name <- object_names[i]
    
    if (inherits(raster_layer, "Raster")) {
      values <- getValues(raster_layer)
    } else if (inherits(raster_layer, "SpatRaster")) {
      values <- values(raster_layer)
    } else {
      stop("Unsupported raster type: ", class(raster_layer))
    }
    
    # Remove NA values for accurate computation
    values <- na.omit(values)
    
    # Bootstrap confidence intervals
    ci <- calculate_bootstrap_intervals(values)
    
    # Create the report data frame
    raster_report <- data.frame(
      Name = df_name,
      Param_1 = param_1,
      Param_2 = param_2,
      Param_3 = param_3,
      Mean_Value = round(mean(values), 3),
      CI_Mean_Lower = round(ci['mean_lower'], 3),
      CI_Mean_Upper = round(ci['mean_upper'], 3),
      Min_Value = round(min(values), 3),
      CI_Min_Lower = round(ci['min_lower'], 3),
      CI_Min_Upper = round(ci['min_upper'], 3),
      Max_Value = round(max(values), 3),
      CI_Max_Lower = round(ci['max_lower'], 3),
      CI_Max_Upper = round(ci['max_upper'], 3)
    )
    
    print(raster_report)
    return(raster_report)
  })
  
  combined_report <- do.call(rbind, report)
  table_path <- file.path(output_path, paste0(prefix, "gridded_rasters_report_", suffix, ".csv"))
  write.table(combined_report, table_path, sep = ",", row.names = FALSE, col.names = !file.exists(table_path), append = TRUE)
}




fx_combine_yearly_rasters <- function(path, pattern, 
                                      grid_output_path = geo_output_path,
                                      output_filename = "cumulative",
                                      idw_var_name = "EHE / ECE / IDW",
                                      plot_ranges = plot_ranges) {
  
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and create a list of rasters
  raster_list <- lapply(files, rast)
  
  dates <- substr(files, 0L , 4L)
  
  # Combine the list of rasters into a SpatRaster object
  combined_rasters <- do.call(c, raster_list)
  
  names(combined_rasters) <- as.list(dates)
  
  cumulative_grid_terra <- app(combined_rasters, sum, na.rm = TRUE)
  names(cumulative_grid_terra) <- "cumulative"
  
  terra::writeRaster(combined_rasters,
                     file.path(grid_output_path, 
                               paste0(output_filename,
                                      "_stack_yearly",
                                      ".tif")),
                     filetype = "GTiff",
                     overwrite=TRUE)
  
  terra::writeRaster(cumulative_grid_terra,
                     file.path(grid_output_path, 
                               paste0(output_filename,
                                      "_cumulative_yearly",
                                      ".tif")),
                     filetype = "GTiff",
                     overwrite=TRUE)
  
  cumulative_event_coverage <- length(cumulative_grid_terra[!is.na(cumulative_grid_terra)]) # // coverage ----
  
  fx_cumulative_impact_plot(grid_output_path,
                            cumulative_grid_terra, 
                            map_divisions = states_geo,
                            year = "cumulative",
                            output_filename = output_filename,
                            cumulative_event_coverage = cumulative_event_coverage,
                            idw_var_name = idw_var_name,
                            plot_ranges = plot_ranges)

  fx_reportRasterMetadata(
    cumulative_grid_terra,
    
    prefix = "",
    suffix = "study_period_yearly",
    output_path = yearly_dataset_path,
    param_1 = idw_var_name,
    param_2 = "cumulative",
    param_3 = cumulative_event_coverage)
  
  outputs_object <- list(combined_rasters, 
                         cumulative_grid_terra,
                         cumulative_event_coverage)
  return(outputs_object)
}
