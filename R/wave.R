get_current_wave_dir <- function(calib_object) {
  fs::path(
    calib_object$config$root_directory,
    "waves",
    calib_object$state$wave
  )
}

update_wave_iteration <- function(calib_object) {
  if (calib_object$state$done) {
    calib_object <- increment_wave(calib_object)
  } else {
    calib_object$state$iteration <- calib_object$state$iteration + 1
    if (calib_object$state$iteration > calib_object$config$max_iteration) {
      save_calib_object(calib_object)
      stop("Max iteration exceded, stopping.")
    }
  }
  calib_object
}

increment_wave <- function(calib_object) {
  clear_sideloads(calib_object)

  if (calib_object$state$wave == length(calib_object$waves)) {
    return(calib_object)
  }

  calib_object$state$done <- FALSE
  calib_object$state$iteration <- 1
  calib_object$state$wave <- calib_object$state$wave + 1
  calib_object$state$jobs_done <-
    rep(FALSE, length(calib_object$waves[[calib_object$state$wave]]))

  make_directories(calib_object)
  calib_object
}
