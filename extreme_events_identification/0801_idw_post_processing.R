library(data.table)
library(ggplot2)
library(raster)
library(terra)
library(tidyterra)

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

# Create the Raster object that is needed for computing cumulative & differential stats, not suitable for visualization
tic()
# IWD 2 Raster ----
idw_ehce_raster_bbox <- raster::raster(interpolated_idw_SP["var1.pred"])
#plot(idw_ehce_raster_bbox)
#hist(idw_ehce_raster_bbox)
names(idw_ehce_raster_bbox) <- selected_day

saveRDS(idw_ehce_raster_bbox,
        file.path(rds_output_path,
                  paste0("idw_ehce_raster_bbox_",
                         selected_day,
                         ".rds")))

terra::writeRaster(idw_ehce_raster_bbox,
                   file.path(geo_output_path,
                             paste0("idw_ehce_raster_bbox_",
                                    selected_day)),
                   filetype = "raster",
                   overwrite=TRUE)
fx_toc(idw_ehce_raster_bbox)

tic()
#idw_ehce_raster <- mask(idw_ehce_raster_bbox,
#                        masking_layer)
idw_ehce_raster <- raster::mask(raster::crop(idw_ehce_raster_bbox,
                                masking_layer), masking_layer)
rm(idw_ehce_raster_bbox) ## remove raster bbox object ----

idw_ehce_raster <- trim(idw_ehce_raster)

saveRDS(idw_ehce_raster,
        file.path(rds_output_path,
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
idw_ehce_raster_matrix <- raster::as.matrix(idw_ehce_raster)
saveRDS(idw_ehce_raster_matrix,
        file.path(rds_output_path,
                  paste0("idw_ehce_raster_matrix_",
                         selected_day,
                         ".rds")))

table_path <- file.path(table_output_path, 
                        paste0("idw_ehce_raster_matrix_",
                               selected_day,
                               ".csv"))

write.table(idw_ehce_raster_matrix, table_path, 
            sep = ",", row.names = F, append = F,
            col.names = !file.exists(idw_ehce_dt))
fx_toc(idw_ehce_raster_matrix)

rm(idw_ehce_raster_matrix)  ## remove matrix from raster object ----

tic()
idw_ehce_dt <- as.data.frame(idw_ehce_raster, xy=TRUE)
setDT(idw_ehce_dt)

saveRDS(idw_ehce_dt,
        file.path(rds_output_path,
                  paste0("idw_ehce_dt_",
                         selected_day,
                         ".rds")))

fx_toc(idw_ehce_dt)
rm(idw_ehce_dt) ## remove data.table object ----

tic()
idw_ehce_raster_unified <- idw_ehce_raster

#idw_ehce_raster_unified[(idw_ehce_raster_unified < EHCE_threshold) & (idw_ehce_raster_unified > -0.2)] <- NA
idw_ehce_raster_unified[(idw_ehce_raster_unified == 0)] <- NA
idw_ehce_raster_unified[!is.na(idw_ehce_raster_unified)] <- 1L
names(idw_ehce_raster_unified) <- selected_day

fx_toc(idw_ehce_raster_unified)


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

# writeCDF(idw_ehce_terra_raster, 
#             file.path(geo_output_path, "netcdf.nc"),
#             varname=paste0("layer_", selected_day), 
#             overwrite=TRUE)
fx_toc(idw_ehce_terra_raster)


rm(idw_ehce_raster) ## remove raster masked object ----
rm(interpolated_idw_SP) ## remove sp object ----


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

  plot_file_name <- paste0(output_path, "/",
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
  
