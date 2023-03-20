# A sideload is data stored by a job. It can be used to keep track of the
# previous calibration iteration or to communicate between the proposer and the
# completion checker.

#' Save some data to be reused by the calibration process
#' @export
save_sideload <- function(calib_object, job, x) {
  sl_path <- get_sideload_path(calib_object, job)
  saveRDS(x, sl_path)
}

#' Read some data saved to be reused by the calibration process
#' @export
load_sideload <- function(calib_object, job) {
  sl_path <- get_sideload_path(calib_object, job)
  if (!fs::file_exists(sl_path)) {
    return(NULL)
  } else {
    readRDS(sl_path)
  }
}

get_sideload_path <- function(calib_object, job) {
  sideload_directory <- get_sideload_dir(calib_object)
  fs::path(sideload_directory, paste0(get_job_id(job), ".rds"))
}

get_sideload_dir <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "sideloads")
}

