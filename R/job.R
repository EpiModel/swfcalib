initialize_job_ids <- function(calib_object) {
  for (wave_i in seq_along(calib_object$waves)) {
    for (job_i in seq_along(calib_object$waves[[wave_i]])) {
      calib_object$waves[[wave_i]][[job_i]]$id <-
        paste0("wave", wave_i, "-job", job_i)
    }
  }
  calib_object
}

get_current_jobs <- function(calib_object, not_done_only = FALSE) {
  current_wave <- get_current_wave(calib_object)
  current_jobs <- calib_object$waves[[current_wave]]
  if (not_done_only) {
    current_jobs <- current_jobs[!get_jobs_done_status(calib_object)]
  }
  current_jobs
}

get_job_id <- function(job) {
  job$id
}

