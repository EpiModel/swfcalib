#' First calibration step
#'
#' @param calib_object a formatted calibration object
#' @param n_cores number of cores to run the processing on
#'
#' @export
calibration_step1 <- function(calib_object, n_cores) {
  oplan <- future::plan("multicore", workers = n_cores)
  on.exit(future::plan(oplan), add = TRUE)

  calib_object <- load_calib_object(calib_object)

  calib_object <- process_sim_results(calib_object)
  results <- load_results(calib_object)
  update_assessments(calib_object, results)

  calib_object <- update_calibration_state(calib_object, results)

  if (is_calibration_complete(calib_object)) {
    # When the calibration is done, skip the next step
    next_step <- slurmworkflow::get_current_workflow_step() + 2
    slurmworkflow::change_next_workflow_step(next_step)
    message("Calibration complete")
  } else {
    proposals <- make_proposals(calib_object, results)
    save_proposals(calib_object, proposals)
    save_calib_object(calib_object)
  }

  print_log(calib_object)
}

#' Second calibration step: run the model for each proposal
#'
#' @param batch_num the batch number for the current proposal
#' @param n_batches the total number of batches for this step
#'
#' @inheritParams calibration_step1
#'
#' @export
calibration_step2 <- function(calib_object, n_cores, batch_num, n_batches) {
  oplan <- future::plan("multicore", workers = n_cores)
  on.exit(future::plan(oplan), add = TRUE)

  calib_object <- load_calib_object(calib_object)
  proposals <- load_proposals(calib_object)

  seq_sim <- get_seq_sim(calib_object, batch_num, n_cores)

  future.apply::future_lapply(
    seq_sim,
    function(i, calib_object, proposals) {
      proposal <- get_proposal_n(proposals, i)
      sim_result <- run_simulation(calib_object, proposal)
      save_sim_result(calib_object, sim_result, i, proposal)
    },
    calib_object = calib_object,
    proposals = proposals,
    future.seed = TRUE
  )

  # last batch set the workflow to go back one step
  if (batch_num == n_batches) {
    next_step <- slurmworkflow::get_current_workflow_step() - 1
    slurmworkflow::change_next_workflow_step(next_step)
  }
}

#' Third calibration step: Wrap up the calibration system and store the results
#'
#' Having this as a separate step allows a mail to be send at the end of the
#' calibration
#'
#' @inheritParams calibration_step1
#'
#' @export
calibration_step3 <- function(calib_object) {
  wrap_up_calibration(load_calib_object(calib_object))
}
