get_seq_sim <- function(calib_object, batch_num, batch_size) {
  n_sims <- calib_object$config$n_sims
  lower_bound <- (batch_num - 1) * batch_size + 1
  upper_bound <- min((batch_num * batch_size), n_sims)
  seq(lower_bound, upper_bound)
}

get_batch_numbers <- function(calib_object, batch_size) {
  n_batch <- ceiling(calib_object$config$n_sims / batch_size)
  seq_len(n_batch)
}

update_calibration_state <- function(calib_object, results) {
  # Do not check the results on the first iteration
  if (calib_object$state$iteration > 0) {
    jobs_results <- get_jobs_results(calib_object, results)
    calib_object$state$done <-
      !any(vapply(jobs_results, is.null, logical(1)))
    calib_object <- update_default_proposal(calib_object, jobs_results)
  }
  calib_object <- update_wave_iteration(calib_object)
  calib_object
}

is_calibration_complete <- function(calib_object) {
  calib_object$state$wave == length(calib_object$waves) &&
    calib_object$state$done
}
