run_simulation <- function(calib_object, proposal) {
  calib_object$config$simulator(proposal)
}

print_log <- function(calib_object) {
  cat("Current state:\n")
  cat("\tWave: ", get_current_wave(calib_object), "\n")
  cat("\tIteration: ", get_current_iteration(calib_object), "\n\n")
  cat("The `default_proposal` currently contains:\n")
  default_proposal <- get_default_proposal(calib_object)
  for (nm in names(default_proposal)) {
    cat("\t", nm, ": ", default_proposal[[nm]][1], "\n")
  }
}
