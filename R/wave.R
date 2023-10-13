get_current_wave <- function(calib_object) {
  calib_object$state$wave
}

get_current_wave_dir <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  current_wave <- get_current_wave(calib_object)
  fs::path(root_directory, "waves", current_wave)
}

is_valid_wave <- function(calib_object) {
  get_current_wave(calib_object) <= length(calib_object$waves)
}

is_wave_done <- function(calib_object) {
  calib_object$state$done
}

update_wave_iteration <- function(calib_object) {
  if (is_wave_done(calib_object)) {
    calib_object <- increment_wave(calib_object)
  } else {
    calib_object <- increment_iteration(calib_object)
    if (!is_valid_iteration(calib_object)) {
      save_calib_object(calib_object)
      stop("Max iteration exceded, stopping.")
    }
  }
  calib_object
}

increment_wave <- function(calib_object) {
  current_wave <- get_current_wave(calib_object)
  calib_object <- mutate_done_status(calib_object, FALSE)
  calib_object <- mutate_calib_state(calib_object, "iteration", 1)
  calib_object <- mutate_calib_state(calib_object, "wave", current_wave + 1)
  clear_sideloads(calib_object)
  make_directories(calib_object)
  calib_object
}

