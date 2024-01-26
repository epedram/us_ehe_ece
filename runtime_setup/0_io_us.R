# Latest update Aug 2023

# ---- # Define specific folders for input, interim and output (IO)

# I/O -----
source_dir <- "/n/groups/patel/pedram/us_climate_data"

#scratch_dir <- "/n/scratch3/users/p/pef004/us_climate_data"
scratch_dir <- "/n/scratch/users/p/pef004/us_climate_data_s"

archive_dir <- "/n/groups/patel/pedram/us_climate_data"

st_label <- paste(geography, time_period, sep = "_")
l1_label <- paste(project_title, task_title, sep = "_")
l2_label <- paste(project_title, time_period, sep = "_")
l3_label <- paste(task_title, time_period, sep = "_")

l4_label <- paste(l1_label, geography, sep = "_")
l5_label <- paste(l1_label, time_period, sep = "_")
l6_label <- paste(l4_label, time_period, sep = "_")

project_prefix_label <- l1_label


cat(st_label, #Spatial_Temporal
    l1_label, l2_label, l3_label,
    l4_label, l5_label, l6_label,
    sep = "\n")

print(Sys.time())
timestamp <- format(Sys.time(), "_@_%y%m%d_%H%M") # YYMMDD_HHMM
print(timestamp)


# define the runtime collector folder
project_task_folder <- paste(project_title, 
                             task_title, sep = "_")

if(is.null(runtime_params)){
    runtime_params <- "" }

if(pointer == "archive") {
    # collect results of final runtime
    dir.create(file.path(archive_dir, project_task_folder))
    reports_path <- file.path(archive_dir, project_task_folder)
    
    timestamped_folder <- ""
    
} else {
        # collect results of test runtimes
        if (is.null(job_id)) {
        timestamped_folder <- paste(st_label, # Essential spatial and temporal tags
                                              # Other potential tags
                                      runtime_params,
                                      timestamp, 
                                      sep = "_")
        } else {
        timestamped_folder <- paste(st_label,
                                    runtime_params,
                                    "O2_JobID", job_id, sep = "_")
        }
    dir.create(file.path(scratch_dir, project_task_folder))
    reports_path <- file.path(scratch_dir, project_task_folder)
}

dir.create(file.path(reports_path, timestamped_folder))
runtime_path <<- file.path(reports_path, timestamped_folder)

    dir.create(file.path(reports_path, timestamped_folder, "Geo"))
    dir.create(file.path(reports_path, timestamped_folder, "Tables"))
    dir.create(file.path(reports_path, timestamped_folder, "Metadata"))
    dir.create(file.path(reports_path, timestamped_folder, "Plots"))
    dir.create(file.path(reports_path, timestamped_folder, "R_Objects"))
    
    geo_output_path <- file.path(runtime_path, "Geo")
    tables_output_path <- file.path(runtime_path, "Tables")
    meta_output_path <- file.path(runtime_path, "Metadata")
    plots_output_path <- file.path(runtime_path, "Plots")
    rds_output_path <- file.path(runtime_path, "R_Objects")

writeLines(capture.output(sessionInfo()),
           file.path(runtime_path,
                     paste0("01_sessions_lib_info",
                     timestamp, 
                     ".txt")))


zip(file.path(runtime_path,
              paste0("00_source_codes_",
                     runtime_params, "_",
                     timestamp, "_",
              job_id,
              ".zip")),
    files = c(here("runtime_setup"),
              here("i_spatial_data_preprocessing"),
              here("ii_extreme_events_identification"),
              here("iii_external_data_integration")
              ),
    extras = "-x \\*Report*")

gc()

fx_io()
