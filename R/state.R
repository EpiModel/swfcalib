get_current_iteration <- function(calib_object) {
  calib_object$state$iteration
}

get_max_iteration <- function(calib_object) {
  calib_object$config$max_iteration
}

get_n_sims <- function(calib_object) {
  calib_object$config$n_sims
}

get_seq_sim <- function(calib_object, batch_num, batch_size) {
  n_sims <- get_n_sims(calib_object)
  lower_bound <- (batch_num - 1) * batch_size + 1
  upper_bound <- min((batch_num * batch_size), n_sims)
  seq(lower_bound, upper_bound)
}

get_batch_numbers <- function(calib_object, batch_size) {
  n_batch <- ceiling(get_n_sims(calib_object) / batch_size)
  seq_len(n_batch)
}

increment_iteration <- function(calib_object) {
  current_iteration <- get_current_iteration(calib_object)
  mutate_calib_state(calib_object, "iteration", current_iteration + 1)
}

decrement_iteration <- function(calib_object) {
  current_iteration <- get_current_iteration(calib_object)
  mutate_calib_state(calib_object, "iteration", current_iteration - 1)
}

mutate_done_status <- function(calib_object, done) {
  mutate_calib_state(calib_object, "done", done)
}

mutate_calib_state <- function(calib_object, item, value) {
  calib_object$state[[item]] <- value
  calib_object
}


update_done_status <- function(calib_object, job_results) {
  done <- !any(vapply(job_results, is.null, logical(1)))
  mutate_done_status(calib_object, done)
}

update_calibration_state <- function(calib_object, results) {
  # Do not check the results on the first iteration
  if (get_current_iteration(calib_object) > 0) {
    jobs_results <- get_jobs_results(calib_object, results)
    calib_object <- update_done_status(calib_object, jobs_results)
    calib_object <- update_default_proposal(calib_object, jobs_results)
  }
  calib_object <- update_wave_iteration(calib_object)
  calib_object
}

is_valid_iteration <- function(calib_object) {
  get_current_iteration(calib_object) <= get_max_iteration(calib_object)
}

is_calibration_complete <- function(calib_object) {
  !is_valid_wave(calib_object)
}
