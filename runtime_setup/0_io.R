# Latest update May 2023

# ---- # Define specific folders for input, interim and output (IO)
# I/O -----
source_dir <- "/n/groups/patel/pedram/climate_data/"

scratch_dir <- "/n/scratch3/users/p/pef004/climate_data/"

st_label <- paste(geography, year, sep = "_")
l1_label <- paste(project_title, task_title, sep = "_")
l2_label <- paste(project_title, year, sep = "_")
l3_label <- paste(task_title, year, sep = "_")

l4_label <- paste(l1_label, geography, sep = "_")
l5_label <- paste(l1_label, year, sep = "_")
l6_label <- paste(l4_label, year, sep = "_")

project_prefix_label <- l1_label

cat(st_label, #Spatial_Temporal
    l1_label, l2_label, l3_label,
    l4_label, l5_label, l6_label,
    sep = "\n")



print(Sys.time())
timestamped <- format(Sys.time(), "_@_%y%m%d_%H%M") # YYMMDD_HHMM
print(timestamped)
gc()

project_task_folder <<- l1_label

# define the collector folder
dir.create(file.path(scratch_dir, project_task_folder))
reports_path <- file.path(scratch_dir, project_task_folder)
# can collect derivatives of multiple runtime

# define the runtime collector folder
if (is.null(job_id)) {
timestamped_folder <- paste(st_label, # Essential spatial and temporal tags
                                      # Other potential tags
                              timestamped, sep = "_")
} else {
timestamped_folder <- paste(st_label,
                              "O2_JobID", job_id, sep = "_")
}

dir.create(file.path(reports_path, timestamped_folder))
runtime_path <<- file.path(reports_path, timestamped_folder)

dir.create(file.path(reports_path, timestamped_folder, "Geo"))
dir.create(file.path(reports_path, timestamped_folder, "Tables"))
dir.create(file.path(reports_path, timestamped_folder, "Metadata"))
dir.create(file.path(reports_path, timestamped_folder, "Plots"))
dir.create(file.path(reports_path, timestamped_folder, "R_Objects"))

geo_output_path <<- file.path(runtime_path, "Geo")
tables_output_path <<- file.path(runtime_path, "Tables")
meta_output_path <<- file.path(runtime_path, "Metadata")
plots_output_path <<- file.path(runtime_path, "Plots")
rds_output_path <<- file.path(runtime_path, "R_Objects")

zip(paste0(runtime_path,
           "/00_sources_code",
           timestamped, ".zip"),
    files = c(#here("runtime_setup"),
              #here("bash_scripts"),

              here("extreme_events_identification")),

    extras = "-x \\*Report*")


writeLines(capture.output(sessionInfo()),
           paste0(runtime_path,
                  "/01_sessions_lib_info",
                  timestamped, ".txt"))
