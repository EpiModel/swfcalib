initialize_job_ids <- function(calib_object) {
  for (wave_i in seq_along(calib_object$waves)) {
    for (job_i in seq_along(calib_object$waves[[wave_i]])) {
      calib_object$waves[[wave_i]][[job_i]]$id <-
        paste0("waves", wave_i, "-job", job_i)
    }
  }
  calib_object
}

get_current_jobs <- function(calib_object) {
  current_wave <- get_current_wave(calib_object)
  calib_object$waves[[current_wave]]
}

get_job_id <- function(job) {
  job$id
}

