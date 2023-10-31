get_root_dir <- function(calib_object) {
  calib_object$config$root_directory
}

make_directories <- function(calib_object) {
  dirs <- c(
    get_sim_result_save_dir(calib_object),
    get_sideload_dir(calib_object)
  )
  for (d in dirs) {
    if (!fs::dir_exists(d)) fs::dir_create(d)
  }
  invisible(TRUE)
}

# calib_save_path must be a variable in all SWF steps
load_calib_object <- function(calib_object) {
  save_path <- get_calib_object_path(calib_object)
  if (fs::file_exists(save_path)) {
    calib_object <- readRDS(save_path)
  } else {
    message("No calib_object file found. initializing the state and returning")
    calib_object <- initialize_state(calib_object)
    make_directories(calib_object)
  }
  calib_object
}


get_calib_object_path <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "calib_object.rds")
}

save_calib_object <- function(calib_object) {
  saveRDS(calib_object, get_calib_object_path(calib_object))
}

initialize_state <- function(calib_object) {
  calib_object$state <- list(
    done = FALSE,
    wave = 1,
    iteration = 0,
    default_proposal = calib_object$config$default_proposal
  )
  calib_object <- initialize_job_ids(calib_object)
  calib_object <- mutate_jobs_done_status(
    calib_object,
    rep(FALSE, length(get_current_jobs(calib_object)))
  )
  calib_object
}
