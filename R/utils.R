run_simulation <- function(calib_object, proposal) {
  calib_object$config$simulator(proposal)
}

print_log <- function(calib_object) {
  cat("Current state:\n")
  cat("\tWave: ", calib_object$state$wave, "\n")
  cat("\tIteration: ", calib_object$state$iteration, "\n\n")
  cat("The `default_proposal` currently contains:\n")
  default_proposal <- calib_object$state$default_proposal
  for (nm in names(default_proposal)) {
    cat("\t", nm, ": ", default_proposal[[nm]][1], "\n")
  }
}
