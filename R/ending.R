get_calibrated_csv_path <- function(calib_object) {
  fs::path(calib_object$config$root_directory, "calibrated.csv")
}

get_param_type <- function(x) {
  dplyr::case_when(
    is.logical(x) ~ "logical",
    is.numeric(x) ~ "numeric",
    .default = "character"
  )
}

save_calibrated_csv <- function(calib_object) {
  long_calib <- get_long_calibrated_proposal(calib_object)
  write.csv(long_calib, get_calibrated_csv_path(calib_object), row.names = FALSE)
}

wrap_up_calibration <- function(calib_object) {
  calib_object$calibrated_proposal <- calib_object$state$default_proposal
  save_calibrated_csv(calib_object)

  full_results <- make_full_results(calib_object)
  save_full_results(calib_object, full_results)

  save_calib_object(calib_object)
  message("Calibration complete")
  print_log(calib_object)
}
