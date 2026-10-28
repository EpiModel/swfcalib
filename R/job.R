initialize_job_ids <- function(calib_object) {
  for (wave_i in seq_along(calib_object$waves)) {
    for (job_i in seq_along(calib_object$waves[[wave_i]])) {
      calib_object$waves[[wave_i]][[job_i]]$id <-
        paste0("wave", wave_i, "-job", job_i)
    }
  }
  calib_object
}
