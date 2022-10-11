#' @export
calibration_step1 <- function(calib_object, n_cores) {
  oplan <- future::plan("multicore", workers = n_cores)
  on.exit(future::plan(oplan), add = TRUE)

  calib_object <- load_calib_object(calib_object)

  calib_object <- process_sim_results(calib_object)
  results <- load_results(calib_object)
  calib_object <- update_calibration_state(calib_object, results)

  if (is_calibration_complete(calib_object)) {
    wrap_up_calibration(calib_object)
  } else {
    proposals <- make_proposals(calib_object, results)
    save_proposals(calib_object, proposals)
    save_calib_object(calib_object)
  }

  print_log(calib_object)
}

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

#' @export
calibration_step3 <- function(calib_object) {
  calib_object <- load_calib_object(calib_object)
  full_results <- load_full_results(calib_object)
  save_full_results(calib_object, full_results)
  print_log(calib_object)
}
