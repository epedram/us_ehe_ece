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
}

# some benchmarks for measure the raster size
fx_toc <- function(x) {
  name <- substitute(x)
  ln <- length(x)
  length <- paste0(scales::label_number(accuracy = 0.001, scale_cut = scales::cut_short_scale())(ln))
  dm <- dim(x)
  cl <- class(x)
  ty <- typeof(x)
  
  msize <- format(object.size(x), units = "auto")
  methods <- methods(class = cl)
  timing <- toc(quiet = TRUE)$callback_msg

  cat("\n Object Name: ", name,
      "\n Object Class: ", cl,
      "\n Object Type: ", ty, 
      "\n Processing Time: ", timing,
      "\n Size in Memory: ", msize,
      "\n Length: ", length,
      "\n Dimension: ", dm,
      "\n"
  )
  
  tb_report <- cbind(name,
                     cl, ty,
                     timing, msize, length)
  table_path <- paste0(meta_output_path, "/",
                   "processed_idw_report",
                   ".csv")
  
  write.table(tb_report, table_path, 
              sep = ",", row.names = F, append = T,
              col.names = !file.exists(table_path))
  # tryCatch({
  # nlayers <- nlayers(x)
  # crs <- crs(x)
  # prj <- proj4string(x)
  #sm <- summary(x)
  #st <- str(x)
  
  # cat("\n Layers: ", nlayers,
  #     "\n CRS: ", crs,
  #     "\n PRJ: ", prj
  #     #"\n Methods by class :", methods,
  #     #"\n Methods by keyword :", installed_methods
  # )
  # }, error = function(e){
  #   cat(selected_day,
  #       "ERROR :",conditionMessage(e), "\n")
  # })
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


