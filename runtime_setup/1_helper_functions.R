# Latest update Oct 2022

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

fx_NamedList <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

fx_combine_rds <- function(path, pattern){
  # Get list of files matching pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read each file and combine into single data table
  combined_dt <- purrr::map_dfr(files, readRDS)
  
  return(combined_dt)
}


fx_normalize_by_range <- function(x)
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
                   comment1 = "comment_1", comment2 = "comment_2") {
  name <- substitute(x)
  
  ln <- length(x)
  ln_scaled <- paste0(scales::label_number(accuracy = 0.1, 
                                        scale_cut = scales::cut_short_scale())(ln))
  ln_scaled_nodecimal <- scales::label_number_si()(ln)
  ln_formatted <- scales::label_comma()(ln)

  dm <- dim(x)
  dm_1 <- dm[1]
  dm_2 <- dm[2]
  #dm_3 <- ifelse(dm >= 3, dm[3][1], NA)
  
  cl <- class(x)
  cl_1 <- cl[1]
  cl_2 <- cl[2]
  #cl_2 <- ifelse(cl >= 2, cl[2], NA)
  
  ty <- typeof(x)
  methods <- methods(class = cl)
  
  #msize <- format(object.size(x), units = "auto")
  msize <- scales::label_bytes()(object.size(x)[1])
  
  processing_time <- toc(quiet = TRUE)$callback_msg
  
  tic()
  rds_file_path <- file.path(rds_output_path,
                             paste0(name, "_", comment1,
                                    ".rds"))
  if(switch == 1L){
    saveRDS(x, rds_file_path)
    filesize <- file.info(rds_file_path)$size
    filesize <- scales::label_bytes()(filesize)
    
  } else {filesize = 0}
  
  storing_time <- toc(quiet = TRUE)$callback_msg
  
  tryCatch({
    epsg <- st_crs(x)$epsg
    }, error = function(e){
    epsg <- NA
  })

  cat("\n\n Object Name: ", name,
      "\n Object Class: ", cl,
      "\n Object Type: ", ty, 
      "\n Length: ", ln_formatted,
      "\n Dimension: ", dm,
      "\n Dim_1: ", dm_1,
      "\n Dim_2: ", dm_2,
      #"\n Dim_3: ", dm_3,
      "\n Comment: ", comment1,
      "\n Processing Time: ", processing_time,
      "\n Size in Memory: ", msize,
      "\n Storing Time: ", storing_time,
      "\n Size on Disk: ", filesize, 
      "\n "
  )

  tb_report <- cbind(name,
                     cl_1, cl_2, ty,
                     processing_time, msize, 
                     storing_time, filesize,
                     ln,
                     #ln_formatted,
                     ln_scaled,
                     dm_1, dm_2, 
                     comment1,
                     #epsg,
                     comment2)
  
  table_path <- paste0(runtime_path, "/",
                   "idw_post_processing_report",
                   ".csv")
  
  write.table(tb_report, table_path, 
              sep = ",", row.names = F, append = T,
              col.names = !file.exists(table_path))
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
              paste0(output_path, "/",
                     prefix, "_",
                     df_name, "_",
                     suffix, "_",

                     nrow(tb), "x", length(tb),
                     ".csv"),
              row.names = F)
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

########################################################
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

########################################################
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

########################################################
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
    #number_of_infs <- sapply(tb, function(x) sum(is.infinite(x)))

    df_name = object_names[i]

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

#######################################
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


########################################################
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


# f(x): Write a csv file (with added row numbers) on a certain folder
fx_varSum <- function(df,
                       fileds = selected_fields,
                       param = k_param, scale = suffix,
                       project_name = project_name){

df_summary_stats <- df %>%
  st_drop_geometry() %>%
  summarise_at(fileds,

               funs(
                 .median = round(median(.,na.rm = TRUE), 2),
                 .mean = round(mean(.,na.rm = TRUE), 2),
                 .sd = round(sd(.,na.rm = TRUE), 2),
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
  mutate(scale = scale) %>%
  mutate(nrow = nrow(df)) %>%
  mutate(param = param) %>%
  mutate(project_name = project_name)

return(df_summary_stats)
}

######################################################## Keep this
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

# Visualization functions----
fx_ehe_ece_iwd_plot <- function(terralayer, output_path,
                                selected_day = selected_day,
                                n = impacted_stations,
                                caption = "Data Source: NOAA Integrated Surface Database (ISD)") {
  
  interpolated_EHCE_plot <-  ggplot() +
    geom_spatraster(data = terralayer) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, na.value = "white") +
    #
    geom_sf(data = us_census_pop,
            fill = NA,
            color = "Black",
            lwd = .4) +
    
    geom_sf(data = idw_ehce_contour,
            color = "Black",
            lwd = .3) +
    
    labs(title = paste0("Extreme Event Date: ",  selected_day)) +
    labs(subtitle = paste0(n, " station(s) impacted within Contiguous U.S.")) +# (", year, ")")) +
    labs(caption = caption) +
    
    geom_sf(data = station_points_SF,
            color = "Black",
            size = .2)  #+
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
  print(fx_toc(interpolated_EHCE_plot, 0, selected_day_label))
}
