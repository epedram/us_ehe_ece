# Latest update Oct 2023

## ---- R helper functions
# helper functions

# f(x): 2022-10 - Print the project I/O set up
fx_io <- function()
{
  cat("I/O Directories:", "\n")
  cat("Data source folder:", "\n")
  print(source_dir)


  cat("\n", "Reporting folders:", "\n")
  #print(param_path)
  print(timestamped_folder)
  print(reports_path)


  #cat("\n", "Outputs folders:", "\n")
  #print(output_path)

  # cat("\n", "Thematic reporting folders:", "\n")
  # print(tables_output_path)
  # print(geo_output_path)
  # print(plots_output_path)
  # print(rds_output_path)
}

fx_info <- function(x) {
  name <- substitute(x)
  ln <- length(x)
  dm <- dim(x)
  nr <- nrow(x)
  nc <- ncol(x)
  
  cl <- class(x)
  ty <- typeof(x)
  
  #crs <- crs(x)
  #prj <- proj4string(x)
  
  msize <- format(object.size(x), units = "auto")
  methods <- methods(class = cl)
  installed_methods <- apropos(cl)
  
  st <- str(x)
  #sm <- summary(x)
  
  cat("\n Name: ", name,
      "\n Length: ", ln,
      "\n Dimension: ", dm,
      "\n # Rows: ", nr,
      "\n # Columns: ", nc,
      "\n Class: ", cl,
      "\n Type: ", ty,
      "\n Size in Memory: ", msize
      #"\n PRJ: ", prj
      #"\n CRS: ", crs
      #"\n Methods by class :", methods,
      #"\n Methods by keyword :", installed_methods
  )
  
  tryCatch({
    #  epsg <- st_crs(rm_sf)$epsg
    
    # nlayers <- nlayers(x)
    #  crs <- crs(x)
    #  prj <- proj4string(x)
    #sm <- summary(x)
    #st <- str(x)
    
    # cat("\n Layers: ", nlayers,
    #     "\n CRS: ", crs,
    #     "\n PRJ: ", prj
    #     #"\n Methods by class :", methods,
    #     #"\n Methods by keyword :", installed_methods
    # )
  }, error = function(e){
    #   cat(selected_day,
    #       "ERROR :",conditionMessage(e), "\n")
    nlayers <- NA
    epsg <- NA
    crs <- NA
    prj <- NA
  })
  
}

# some benchmarks for measure the raster size
fx_toc <- function(x, switch = 1L,
                   comment1 = "comment_1", 
                   comment2 = "comment_2",
                   output_path = rds_output_path) {
  
  name <- substitute(x)
  
  tryCatch({
  
  ln <- length(x)
  ln_scaled <- paste0(scales::label_number(accuracy = 0.1, 
                                           scale_cut = scales::cut_short_scale())(ln))
  ln_scaled_nodecimal <- paste0(scales::label_number(accuracy = 0, 
                                                     scale_cut = scales::cut_short_scale())(ln))
  #ln_scaled_nodecimal <- scales::label_number_si()(ln)
  
  ln_formatted <- scales::label_comma()(ln)
  
  dm <- dim(x)
  dm_all <- paste(dim(x), collapse = " x ")
  # Format dimensions with 3-digit separator
  dm_0 <- paste(sapply(dim(x), function(d) format(d, big.mark = ";", scientific = FALSE)), collapse = "x")
  
  # Split the formatted dimensions
  formatted_dimensions <- strsplit(dm_0, "x")[[1]]
  
  # Extract individual dimensions with default "NA" if not available
  dm_1 <- formatted_dimensions[1]
  dm_2 <- ifelse(length(formatted_dimensions) >= 2, formatted_dimensions[2], "NA")
  dm_3 <- ifelse(length(formatted_dimensions) >= 3, formatted_dimensions[3], "NA")
  
  cl <- class(x)
  cl_len <- length(class(x))
  cl_1 <- cl[1]
  cl_2 <- ifelse(cl_len >= 2, cl[2][1], "NA")
  cl_3 <- ifelse(cl_len >= 3, cl[3][1], "NA")
  cl_all <- paste0("[",
                   paste(class(x), collapse = "] . ["),
                   "]")
                   
  ty <- typeof(x)
  methods <- methods(class = cl)
  
  #msize <- format(object.size(x), units = "auto")
  mem_size <- scales::label_bytes()(object.size(x)[1])
  
  processing_time <- toc(quiet = TRUE)$callback_msg
  
  tic()
  if(switch == 1L){
    rds_file_path <- file.path(output_path,
                             paste0(name, "_", 
                                    comment1, "_",
                                    comment2,
                                    ".rds"))
    saveRDS(x, rds_file_path)
    
    filesize <- file.info(rds_file_path)$size
    file_size <- scales::label_bytes()(filesize)
    storing_time <- toc(quiet = TRUE)$callback_msg
    
    #if ("raster" %in% tolower(classNames) | "spatial" %in% tolower(classNames)) {
      if (any(sapply(c("Raster", "Spat", "star"),  
                       function(keyword) grepl(keyword, class(x), ignore.case = TRUE)))) {
          # dev a function to report spatial objects info()
        } else { 
        dir.create(file.path(output_path, paste0(".metadata")),
                    recursive = TRUE, showWarnings = FALSE)
         metadata_output_path <- file.path(output_path, paste0(".metadata"))
         
         fx_reportFieldsPar(
           x,
           
           prefix = name,
           suffix = comment1,
           output_path = metadata_output_path)
        }
    
  } else { 
    file_size <- 0
    storing_time <- 0
  }

  tryCatch({ # merge with spatial objects info()
    epsg <- st_crs(x)$epsg
    crs <- st_crs(x)$input
  }, error = function(e){
    epsg <- ""
    crs <- ""
  })
  
  cat("\n\n Object Name: ", name,
      "\n Object Class: ", cl,
      "\n Object Type: ", ty, 
      "\n Length: ", ln_formatted,
      "\n Dimension: ", dm,
      "\n Comment: ", comment1,
      "\n Processing Time: ", processing_time,
      "\n Size in Memory: ", mem_size,
      "\n Storing Time: ", storing_time,
      "\n Size on Disk: ", file_size, 
      "\n EPSG code: ", epsg,
      "\n ")
  
  tb_report <- cbind(name, 
                     cl_all, 
                     dm_all,
                     ln,
                     mem_size, 
                     processing_time, 
                     file_size,
                     storing_time, 
                     #crs, #this can break the csv structure
                     epsg, 
                     #ln_formatted,
                     ty,
                     ln_scaled, 
                     dm_1, dm_2, dm_3, 
                     cl_1, cl_2, cl_3, 
                     comment1,
                     comment2)
  
  table_path <- paste0(runtime_path, "/",
                       "data_processing_report_",
                       time_period,
                       #timestamp,
                       ".csv")
  
  write.table(tb_report, table_path, 
              sep = ",", row.names = F, append = T,
              col.names = !file.exists(table_path))
  
  }, error = function(e){
    cat(name, " fx_toc reporting failed")
    cat(conditionMessage(e))
  })
}

###############################
fx_NamedList <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

fx_combine_csv <- function(path, pattern){
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and combine into single data table
  combined_dt <- purrr::map_dfr(files, fread)
  
  return(combined_dt)
}

fx_combine_rds <- function(path, pattern){
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and combine into single data table
  combined_dt <- purrr::map_dfr(files, readRDS)
  
  return(combined_dt)
}

fx_combine_gpkg <- function(path, pattern){
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and combine into single data table
  combined_dt <- purrr::map_dfr(files, st_read)
  
  return(combined_dt)
}

fx_normalize_by_range <- function(x)
{
  if (min(x) < 0) {
    x <- log (-x + 1)#, base = 10)
    x <- -((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))) %>% 
            round(., 2) * 100
  } else {
    x <- log (x + 1)#, base = 10)
    x <- ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))) %>% 
            round(., 2) * 100
  }  
  return(x)
}



# f(x): 2022-10 - Write a csv file (with added row numbers) on a certain folder
fx_saveCSVSP <- function(sf,
                       output_path = output_path,
                       prefix = "99", suffix = ""){
  xtb <- sf %>% st_drop_geometry()

  write.csv(xtb,
            paste0(output_path, "/",
                   prefix, "_",
                   substitute(sf), "_",
                   suffix, "_",
                   nrow(xtb), "x",
                   length(xtb),
                   ".csv"),

            row.names = TRUE,)
}


fx_saveCSV <- function(x,
                       output_path = output_path,
                       prefix = "99", suffix = ""){
  write.csv(x,
            paste0(output_path, "/",
                   prefix, "_",
                   substitute(x), "_",
                   suffix, "_",
                   nrow(x), "x",
                   ncol(x),
                   ".csv"),

            row.names = TRUE,)
}


# f(x): Append to a csv file (with same number of columns) on a certain folder
fx_appendCSV <- function(x,
                        output_path = output_path,
                        prefix = "", suffix = ""){
  table_path <-
            paste0(output_path, "/",
                   prefix, "_",
                   substitute(x), "_",
                   suffix, "_",
                   length(x),
                   "_columns",
                   ".csv")
  write.table(x, table_path,
  sep = ",", row.names = FALSE,
  col.names = !file.exists(table_path),
  append = T)
}

fx_saveCSVPar <- function(...,
                          output_path = tables_output_path,
                          prefix = "00", suffix = "_metadata") {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)

  lapply(1:length(object_names), function(i) {
    tb <- objects[i][[1]]
    df_name = object_names[i]

    write.csv(tb,
              file.path(output_path,
                        paste0(prefix, "_",
                        df_name, "_",
                        suffix, "_",
                     nrow(tb), "x", length(tb),
                     ".csv")),
              row.names = F)
  }

  )
}

fx_saveGPKG <- function(...,
                          output_path = tables_output_path,
                          prefix = "00", suffix = "_metadata") {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  
  lapply(1:length(object_names), function(i) {
    tb <- objects[i][[1]]
    df_name = object_names[i]
    
    st_write(tb,
             file.path(output_path,
                       paste0(prefix, "_",
                              df_name, "_",
                              suffix, "_",
                              
                              nrow(tb), "x", length(tb),
                              ".gpkg")),
             #                           ".geojson")),
             delete_layer = TRUE)
  }
  
  )
}

# f(x): Write a shapefile (with a subfolder) on a certain folder

fx_saveSHP <- function(sf, output_path = geo_output_path,
                       shpname = "_shapefile",
                       prefix = "", suffix = ""
                       #subfolder = "spatial_layer"
                       ){

  df_name = substitute(sf)

  rgdal::writeOGR(sf::as_Spatial(sf,
                      cast = TRUE),
           layer = paste0(#prefix,
                          df_name,
                          shpname,
                          "_n", nrow(sf)
           ),
           dsn = file.path(output_path
                        #subfolder
                        #suffix
           ),
           driver = "ESRI Shapefile",
           layer_options ="wkbPolygon",
           verbose = FALSE,
           delete_dsn = TRUE, overwrite_layer = TRUE)
}


# f(x): Save ggplot outputs as image files
fx_saveggPlots <- function(gg,
                        folder = output_path,
                        v = "variable",
                        p = "", s = "",
                        w = 20, h = 18
){

  plot_file_name <- paste0(folder, "/",
                           p, "_",
                           #substitute(gg), "_",
                           v, "_",
                           s, "_",
                           w, "x",
                           h,
                           ".jpg")

  ggsave(plot_file_name,
         plot = gg, dpi = 300,
         width = w, height = h,
         units = "cm")
}

# f(x): Collect and Read list of RDS objects
fx_Read_RDS_Objects <- function(input_path = input_path) {
  print(input_path)

  lst <- list.files(path = input_path,
                            pattern = ".rds")
  print(lst)

  sapply(1:length(lst), function(i) {
    nm <- substr(lst[i], 1, (nchar(lst[i])-4))
    print(nm)

    rds_object <- readRDS(file.path(input_path,
                               lst[[i]]))[[1]]
    assign(nm, rds_object, envir = .GlobalEnv)

  })
  return(as.data.frame(lst))
}

####
fx_Read_CSV_Objects <- function(input_path) {
  print(input_path)

  lst <- list.files(path = input_path,
                    pattern = ".csv")
  print(lst)

  sapply(1:length(lst), function(i) {
    nm <- substr(lst[i], 1, (nchar(lst[i])-4))
    print(nm)

    csv_object <- fread(file.path(input_path,
                                    lst[[i]]),
                        header = TRUE, check.names = FALSE) %>%
    as_tibble() %>%
      janitor::clean_names(.)

    assign(nm,
           csv_object, envir = .GlobalEnv)

  })
  return(as.data.frame(lst))
}

####
fx_Read_SHP_Objects <- function(input_path) {
  print(input_path)

  lst <- list.files(path = input_path,
                    pattern = ".shp")
  print(lst)

  sapply(1:length(lst), function(i) {
    nm <- substr(lst[i], 1, (nchar(lst[i])-4))
    print(nm)

    shp_object <- st_read(dsn = file.path(input_path,
                            lst[[i]]),
            stringsAsFactors = FALSE) %>%
            st_as_sf() %>% as_tibble() %>% st_as_sf() %>%
      janitor::clean_names(.)

    assign(nm,
           shp_object, envir = .GlobalEnv)

  })
  return(as.data.frame(lst))
}

####
fx_Read_XLS_Objects <- function(input_path) {
  library(readxl)

  read_excel_allsheets <- function(filename, tibble = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets,
                function(X) readxl::read_excel(filename, sheet = X) %>%
                  janitor::clean_names(.))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    return(x)
    # assign(nm,
    #        x, envir = .GlobalEnv)

  }
  print(input_path)

  lst <- list.files(path = input_path,
                    pattern = ".xls?")
  print(lst)

  sapply(1:length(lst), function(i) {
    nm <- substr(lst[i], 1, (nchar(lst[i])-4))
    print(nm)

    xls_object <- read_excel_allsheets(file.path(input_path,
                                                  lst[[i]]))
    # #print(xls_object)
    assign(nm,
            xls_object, envir = .GlobalEnv)
  })

  return(as.data.frame(lst))
}


# f(x): Create a named list of objects based on the original objects name
# https://stackoverflow.com/questions/18861772/r-get-objects-names-from-the-list-of-objects
fx_createNamedList <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}
#summary(NamedList)



# f(x): Create a csv file presenting data columns and their data type
## https://stackoverflow.com/questions/21125222/determine-the-data-types-of-a-data-frames-columns
fx_fieldsView <- function(x, output_path = table_output_path,
                          prefix = "00", suffix = "metadata"){
  col_class <- sapply(x, class)
  col_type <- sapply(x, typeof)

  df_name = substitute(x)

  write.csv(cbind(col_class, col_type),
               paste0(output_path, "/",
                      prefix, "_",
                      df_name, "_",
                      suffix, "_",
                      nrow(x), "x", length(x),
                      ".csv"),
               row.names = TRUE, )
    }


fx_reportFields <- function(tb,
                            output_path = table_output_path,
                          prefix = "00", suffix = ""){
  col_class <- sapply(tb, class)
  col_type <- sapply(tb, typeof)
  col_index <- 1:dim(tb)[2]
  number_of_nulls <- sapply(tb, function(x) sum(is.na(x)))
  #number_of_infs <- sapply(tb, function(x) sum(is.infinite(x)))
  
  df_name = substitute(tb)

  tb_report <- cbind(
                     col_class, col_type,
                     number_of_nulls,
                     #number_of_infs,
                     col_index
                     )

             write.csv(tb_report,
                       paste0(output_path, "/",
                              prefix, "_",
                              df_name, "_",
                              suffix, "_",
                              "metadata_",

                              nrow(tb), "x", length(tb),
                              ".csv"),
                       row.names = TRUE)
}

fx_reportFieldsPar <- function(...,
                               output_path = tables_output_path,
                               prefix = "00", suffix = "_metadata") {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  #

  lapply(1:length(object_names), function(i) {
    #tb <- objects[[i]][[1]]
    tb <- objects[i][[1]]
    #print(glimpse(tb))

    tb_dim <- dim(tb)[2]
    col_class <- sapply(tb, class)
    col_type <- sapply(tb, typeof)
    col_index <- 1:length(tb)

    number_of_nulls <- sapply(tb, function(x) sum(is.na(x)))
    number_of_infs <- sapply(tb, function(x) sum(is.infinite(unlist(x))))

    df_name = object_names[i]

    tb_report <- cbind(
      col_class, col_type,
      number_of_nulls,
      number_of_infs,
      col_index
    )

    write.csv(tb_report,
              paste0(output_path, "/",
                     prefix, "_",
                     df_name, "_",
                     suffix, "_",

                     nrow(tb), "x", length(tb),
                     ".csv"),
              row.names = TRUE)
  }

  )
}


# f(x): Create a csv file presenting descriptive stats of a dataframe
## https://stackoverflow.com/questions/45176431/extract-name-of-data-frame-in-r-as-character
fx_writeStatsTB <- function(tbdf,
                            output_path = tables_output_path,
                            prefix = "01", suffix = "stats"){
  df_name = substitute(tbdf)
  summary = skim(tbdf)
  print(summary)

  fx_saveCSV(summary,
             output_path = output_path,
             paste0(prefix, "_",
                    df_name, "_"),
                    suffix)
}


fx_writeStatsSP <- function(sfdf,
                            output_path = tables_output_path,
                            prefix = "01", suffix = "stats"){
  df_name = substitute(sfdf)

  summary = skim(sfdf %>% st_drop_geometry())
  print(summary)
  fx_saveCSV(summary,
             output_path = output_path,
             paste0(prefix, "_",
                    df_name, "_"),
                    suffix)
}

# metadata creator for geodb
fx_fgdb_info <- function(fgdb_path){
  x <- sf::st_layers(dsn = fgdb_path)
  layer_name <- x$name
  layer_type <- as.character(x$geomtype)
  n_features <- x$features
  n_fields <- x$fields
  tbt <- as_tibble(cbind(layer_name, layer_type, n_fields, n_features))
  return(tbt)
}

# f(x): Save multiple objects using saveRDS
##https://stackoverflow.com/questions/43304135/save-multiple-objects-using-saverds-and-lapply
# Save each named objects from a given list
fx_saveRObjectsNList <- function(NamedList, output_path = output_path,
                      prefix = "99", suffix = ""){
  #objects <- unlist(...)
  object_names <- names(NamedList)

  sapply(1:length(NamedList), function(i) {
    filename = file.path(output_path = output_path,
                      paste0(
                      prefix, "_",
                      object_names[i], "_",
                      suffix,
                      #"_", class(NamedList[[i]]),
                      #"_", length(NamedList[[i]]),
                      ".rds"))
    nm <- object_names[i]
    #print(i)
    cat(paste0("\n", "Object [",nm,"] of Lenght (",
               length(NamedList[[i]]),
               ") and Class {",
               class(NamedList[[i]]),
               "} saved as:", "\n"))
    #cat(paste0("Class: ",class(lst[[i]]),"\n"))
    print(filename)
    saveRDS(NamedList[i], filename)
  })
}


# Save each object given as a parameter
fx_saveRObjects <- function(...,
                            prefix = "88", suffix = "",
                            output_path = output_path) {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  
  sapply(1:length(objects), function(i) {
    filename = file.path(output_path = output_path,
                         paste0(prefix, "_",
                                object_names[i], "_",
                                suffix,
                                ".rds"))
    
    saveRDS(objects[i], filename)
  })
}


# Save each object given as a parameter // To develop // ----
fx_store_objects <- function(...,
                           prefix = "", suffix = "",
                           output_path = output_path) {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)

  sapply(1:length(objects), function(i) {
    filename = file.path(output_path = output_path,
                         paste0(prefix, "_",
                                object_names[i], "_",
                                suffix,
                                ".rds"))

    saveRDS(objects[i], filename)
  })
}

### 
# f(x): Append prefix or suffix to colnames
# https://stackoverflow.com/questions/35697940/append-suffix-to-colnames
fx_fixColNames <- function(df, prefix="", suffix="", sep="_")
{
  colnames(df) <- paste(prefix, colnames(df), suffix, sep=sep)
  return(df)
}



#rds_list_df <- list.files(path = here(input_dir),
#                          pattern = ".rds")
#fx_ReadRDSObjects(rds_list_df, input_dir)


# f(x): Save plot outputs as image files under
fx_savePlots <- function(
                  visual_name,
                  output_path = plots_output_path,
                  prefix = "90", suffix = "visual",

                  xx = 10,
                  yy = 20,

                  ww = 22,
                  hh = 18){
  plot_file_name <- paste0(output_path, "/",
                           xx, "-", yy,
                           prefix,
                           "-",
                           suffix,
                           ".jpg")
  #https://stackoverflow.com/questions/44711236/set-the-size-of-ggsave-exactly
  #ggsave(plot_file_name, plot = visual_name, dpi = 300,
#         width = ww, height = hh, units = "cm")

  jpeg(plot_file_name,
       width = ww, height = hh)
  print(visual_name)
  dev.off()
}


bitcodes <- function(x) {
  if (x %% 1 != 0) {
    return(NA_real_)
  }
  
  bits <- as.numeric(intToBits(x))
  exponent <- (2^(0:16))
  product <- bits * exponent
  
  non_zero_items <- product[product != 0]
  return(non_zero_items)
}

normalize_by_range <- function(x)
{
  if (min(x) < 0) {
    x <- - x
    x <- - log(1 + round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 3) * 100)
  } else {
    x <- log(1 + round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 3) * 100)
  }
  return(x)
  # }
  #
  # norm_x <- (round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 3) * 100)
  # return(norm_x)
}


fx_validation_summary_stats <- function(df = df,
                             fields = selected_fields,
                             prefix = "+", suffix = "",
                             folder = output_path) {
  library(dplyr)
  library(broom)
  
  summary_stats_df <- df %>%
    dplyr::summarise(n_records = n(),
                     n_stations = n_distinct(station_id),
                     ccoefficient_idw = round(cor(temperature_avg, temperature_avg_62_idw, 
                                                  use = "complete.obs"), 3),
                     ccoefficient_nn = round(cor(temperature_avg, temperature_avg_nn_idw, 
                                                 use = "complete.obs"), 3),
                     ccoefficient_gm = round(cor(temperature_avg, temperature_avg_gridmet, 
                                                 use = "complete.obs"), 3),
                     
                     #p_value_idw_noaa = round(t.test(temperature_avg, temperature_avg_62_idw)$p.value, 5),
                     #p_value_nn_noaa = round(t.test(temperature_avg, temperature_avg_nn_idw)$p.value, 5),
                     #p_value_gm_noaa = round(t.test(temperature_avg, temperature_avg_gridmet)$p.value, 5),
                     
                     p_value_idw_nn = round(t.test(temperature_avg_62_idw, temperature_avg_nn_idw)$p.value, 5),
                     p_value_idw_gm = round(t.test(temperature_avg_62_idw, temperature_avg_gridmet)$p.value, 5),
                     p_value_gm_nn = round(t.test(temperature_avg_gridmet, temperature_avg_nn_idw)$p.value, 5),

                     r2_idw = round(cor(temperature_avg, temperature_avg_62_idw, use = "complete.obs")^2, 3),
                     r2_nn = round(cor(temperature_avg, temperature_avg_nn_idw, use = "complete.obs")^2, 3),
                     r2_gm = round(cor(temperature_avg, temperature_avg_gridmet, use = "complete.obs")^2, 3),
                     
                     improved_R2_idw_over_nn = round(r2_idw - r2_nn, 3),
                     improved_R2_idw_over_gm = round(r2_idw - r2_gm, 3),
                     improved_R2_gm_over_nn = round(r2_gm - r2_nn, 3),

                     across(all_of(selected_fields),
                            list(
                              mean = ~round(mean(., na.rm = TRUE), 2),
                              sd = ~round(sd(., na.rm = TRUE), 2),
                              var = ~round(var(., na.rm = TRUE), 2),
                              median = ~round(median(., na.rm = TRUE), 2),
                              max = ~round(max(., na.rm = TRUE), 2)
                              #min = ~round(min(., na.rm = TRUE), 2),

                            ),
                            .names = "{fn}__{col}")) %>%
    mutate(sample = i) %>% 
    
    # mutate(Improved_by_IDW_over_NN =
    #          round((mean__Abs_Residuals_NN - mean__Abs_Residuals_IDW) / mean__Abs_Residuals_NN, 5)) %>%
    # mutate(Improved_by_IDW_over_GM =
    #          round((mean__Abs_Residuals_GM - mean__Abs_Residuals_IDW) / mean__Abs_Residuals_GM, 5)) %>% 
    
    mutate(RMSE_IDW = round(sqrt(mean__SquaredError_IDW), 3),
           RMSE_NN = round(sqrt(mean__SquaredError_NN), 3),
           RMSE_GM = round(sqrt(mean__SquaredError_GM), 3)) %>% 
    
    mutate(R2_IDW = round(1 - (RMSE_IDW^2 / var__temperature_avg), 3),
           R2_NN = round(1 - (RMSE_NN^2 / var__temperature_avg), 3),
           R2_GM = round(1 - (RMSE_GM^2 / var__temperature_avg), 3)) %>% 

    mutate(R2_Improved_by_IDW_over_NN = round(R2_IDW - R2_NN, 3),
           R2_Improved_by_IDW_over_GM = round(R2_IDW - R2_GM, 3),
           R2_Improved_by_GM_over_NN = round(R2_GM - R2_NN, 3))  #%>%
    # mutate(across(starts_with("r2_"), list(
    #   ci_lower = ~ round(qf((1 - conf.level) / 2, 1, n_records - 2, lower.tail = TRUE, ncp = . * (n_records - 2)) / (n_records - 1 + qf((1 - conf.level) / 2, 1, n_records - 2, lower.tail = TRUE, ncp = . * (n_records - 2))), 3),
    #   ci_upper = ~ round(qf(1 - (1 - conf.level) / 2, 1, n_records - 2, lower.tail = TRUE, ncp = . * (n_records - 2)) / (n_records - 1 + qf(1 - (1 - conf.level) / 2, 1, n_records - 2, lower.tail = TRUE, ncp = . * (n_records - 2))), 3)
    # ), .names = "{col}_{fn}"))
    
    
  stats_filename <- file.path(folder,
                              paste0(
                                prefix,
                                substitute(df), "_[",
                                length(fields),
                                "_columns]_",
                                "stats_(sample",
                                suffix,
                                ").csv"))
  
  write.table(summary_stats_df,
              stats_filename, sep = ",",
              row.names = F, 
              col.names = T,
              append = FALSE)
  
  collecting_stats_filename <- file.path(runtime_path,
                                     paste0(prefix, "_",
                                            substitute(df),
                                            "_stats_all_samples_(", 
                                            time_period,
                                            ").csv"))
  write.table(summary_stats_df,
              collecting_stats_filename, 
              row.names = F, 
              col.names = !file.exists(collecting_stats_filename),
              sep = ",",
              append = T)
  # sort the column names ----
  summary_stats_df_sorted <- summary_stats_df[, order(names(summary_stats_df))]
  
  dir.create(file.path(runtime_path, "_sorted_summaries"), 
             recursive = TRUE, showWarnings = FALSE)
  sorted_summaries_path <- file.path(runtime_path, "_sorted_summaries")
  
  collecting_stats_sorted_filename <- file.path(sorted_summaries_path,
                                         paste0(prefix, "_",
                                                substitute(df),
                                                "_stats_all_samples_(", 
                                                time_period,
                                                ")_sorted.csv"))
  write.table(summary_stats_df_sorted,
              collecting_stats_sorted_filename, 
              row.names = F, 
              col.names = !file.exists(collecting_stats_sorted_filename),
              sep = ",",
              append = T)

  #return(summary_stats_df)
#}
  # Main function
  # fx_validation_summary_stats2 <- function(df, fields, 
  #                                          prefix = "+", suffix = "", 
  #                                          folder = output_path) {
    
  # library(broom) # for tidy statistical summaries
  # 
  # # Helper function to compute paired t-test and retrieve p-value and confidence interval
  #  perform_paired_t_test <- function(observed, model1, model2) {
  #    test_result <- t.test(model1 - observed, model2 - observed, paired = TRUE)
  #    return(tidy(test_result)[, c("estimate", "p.value", "conf.low", "conf.high")])
  #  }
  # 
  #   summary_stats_df2 <- df %>%
  # 
  #      rowwise() %>%
  #      mutate(
  #        idw_nn_test = list(perform_paired_t_test(temperature_avg, temperature_avg_62_idw, temperature_avg_nn)),
  #        idw_gm_test = list(perform_paired_t_test(temperature_avg, temperature_avg_62_idw, temperature_avg_gridmet))
  #      ) %>%
  #      unnest(c(idw_nn_test, idw_gm_test))
  #   
  #   dir.create(file.path(runtime_path, "_sorted_summaries"), 
  #              recursive = TRUE, showWarnings = FALSE)
  #   sorted_summaries_path <- file.path(runtime_path, "_sorted_summaries")
  #   
  #   collecting_stats_refactored_filename <- file.path(sorted_summaries_path,
  #                                                 paste0(prefix, "_",
  #                                                        substitute(df),
  #                                                        "_stats_all_samples_(", 
  #                                                        time_period,
  #                                                        ")_refactored.csv"))
  #   write.table(summary_stats_df2,
  #               collecting_stats_refactored_filename, 
  #               row.names = F, 
  #               col.names = !file.exists(collecting_stats_refactored_filename),
  #               sep = ",",
  #               append = T)
    
    return(summary_stats_df)
  }
  


events_summary_stats <- function(data, group_vars, summary_vars, 
                                 params, rnd = 1) {
  summary_df <- data %>%
    group_by(across(all_of(group_vars))) %>%
    
    summarise(
      Number_of_Records = n(),
      Distinct_Impacted_Stations = n_distinct(station_id),
      Distinct_Impacted_Days = n_distinct(DATE),
      Distinct_Events_Universal = n_distinct(UID, na.rm = TRUE),
      #Distinct_Events_Stations = n_distinct(STEUID, na.rm = TRUE),
      
      #Average_Events_per_Impacted_Station = round((Distinct_Events_Stations / Distinct_Impacted_Stations), 2),
      Average_Days_per_Impacted_Station = round((Distinct_Impacted_Days / Distinct_Impacted_Stations), 2),
      #Average_Impacted_Station_per_Day = round((Distinct_Events_Stations / Distinct_Impacted_Days), 2),
      #Average_Impacted_Station_per_Event = round((Distinct_Impacted_Days / Distinct_Events_Stations), 2),
      
      across(all_of(summary_vars),
             list(
               null = ~sum(is.na(.)),
               inf = ~sum(is.infinite(.)),
               avg = ~round(mean(as.double(.), na.rm = T), rnd),
               min = ~round(min(as.double(.), na.rm = T), rnd),
               max = ~round(max(as.double(.), na.rm = T), rnd),
               median = ~median(as.double(.), na.rm = T),
               sd = ~round(sd(as.double(.), na.rm = T), rnd),
               Q1 = ~round(quantile(., probs = .25, na.rm = TRUE), rnd),
               Q3 = ~round(quantile(., probs = .75, na.rm = TRUE), rnd)
             )),
      .groups = 'drop')
  
  write.csv(summary_df,
            
            file.path(tables_output_path,
                      paste0(substitute(group_vars),
                             "_ehe_ece_summary_",
                             params,
                             "_",
                             nrow(summary_df), "x",
                             ncol(summary_df),
                             ".csv")),
            
            row.names = TRUE)
  
  return(summary_df)
}

# f(x): Write a csv file (with added row numbers) on a certain folder
fx_varSum <- function(df,
                      fields = selected_fields,
                      prefix = "", suffix = "scale",
                      param = "project_name",
                      folder = table_output_path){
  
  df_summary_stats <- df %>%
    st_drop_geometry() %>%
    #dplyr::summarise(n = n(),
    summarise_at(fields,
                 
                 funs(
                   n = n(),
                   median = round(median(.,na.rm = TRUE), 2),
                   mean = round(mean(.,na.rm = TRUE), 2),
                   sd = round(sd(.,na.rm = TRUE), 2),
                   .sum = round(mean(.,na.rm = TRUE), 2),
                   min = round(min(.,na.rm = TRUE), 2),
                   max = round(max(.,na.rm = TRUE), 2),
                   Q1st = round(quantile(., probs = .25, na.rm = TRUE), 2),
                   Q3rd = round(quantile(., probs = .75, na.rm = TRUE), 2),
                   Prcntile15 = round(quantile(., probs = .15, na.rm = TRUE), 3),
                   Prcntile25 = round(quantile(., probs = .25, na.rm = TRUE), 3),
                   Prcntile35 = round(quantile(., probs = .35, na.rm = TRUE), 3),
                   Prcntile45 = round(quantile(., probs = .45, na.rm = TRUE), 3),
                   Prcntile55 = round(quantile(., probs = .55, na.rm = TRUE), 3),
                   Prcntile65 = round(quantile(., probs = .65, na.rm = TRUE), 3),
                   Prcntile75 = round(quantile(., probs = .75, na.rm = TRUE), 3),
                   Prcntile85 = round(quantile(., probs = .85, na.rm = TRUE), 3),
                   Prcntile95 = round(quantile(., probs = .95, na.rm = TRUE), 3)
                 )
    ) %>%
    mutate(nrow = n()) # %>%
  #mutate(scale = scale) %>%
  #mutate(param = param) %>%
  #mutate(project_name = project_name)
  
  return(df_summary_stats)
}
############################################## Keep this at the bottom
# print the list of available helper functions
fx_fxView <- function(pat="fx_")
{
  cat("\n", "List start up objects:", "\n")
  print(ls(pat = pat))
  return(print(ls(pat = pat)))
}

cat("\n", "List start up objects:", "\n")
fx_list <- ls(pat = "fx_")
print(fx_list)



