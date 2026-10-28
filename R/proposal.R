get_proposals_path <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "proposals.rds")
}

#' Get the default proposal of a calibration
#'
#' The default proposal holds the current best value for every parameter under
#' calibration. It starts as `config$default_proposal` and is updated as waves
#' complete, so on a finished calibration it is the calibrated parameter set.
#'
#' @inheritParams calibration_step1
#'
#' @return a one-row `data.frame` of parameter values, one column per calibrated
#'   parameter
#'
#' @export
get_default_proposal <- function(calib_object) {
  calib_object$state$default_proposal
}

get_proposal_n <- function(proposals, n) {
  dplyr::filter(proposals, .data$`.proposal_index` == n)
}

load_proposals <- function(calib_object) {
  proposals_path <- get_proposals_path(calib_object)
  if (fs::file_exists(proposals_path)) {
    readRDS(proposals_path)
  } else {
    dplyr::tibble()
  }
}

save_proposals <- function(calib_object, proposals) {
  saveRDS(proposals, get_proposals_path(calib_object))
  invisible(TRUE)
}

make_proposals <- function(calib_object, results) {
  current_jobs <- calib_object$waves[[calib_object$state$wave]]
  current_jobs <- current_jobs[!calib_object$state$jobs_done]

  if (calib_object$state$iteration == 1) {
    proposals <- lapply(current_jobs, function(job) job$initial_proposals)
  } else {
    proposals <- lapply(
      current_jobs,
      function(co, job, res) job$make_next_proposals(co, job, res),
      res = results,
      co = calib_object
    )
  }
  proposals <- merge_proposals(proposals)
  proposals <- fill_proposals(proposals, calib_object)
  proposals[[".proposal_index"]] <- seq_len(nrow(proposals))
  proposals[[".wave"]] <- calib_object$state$wave
  proposals[[".iteration"]] <- calib_object$state$iteration
  dplyr::select(
    proposals,
    dplyr::everything(), ".proposal_index", ".wave", ".iteration"
  )
}

merge_proposals <- function(proposals) {
  max_rows <- max(vapply(proposals, nrow, numeric(1)))
  proposals <- lapply(
    proposals,
    function(d) {
      missing_rows <- max_rows - nrow(d)
      if (missing_rows > 0) {
        d <- dplyr::bind_rows(
          d,
          dplyr::slice_sample(d, n = missing_rows, replace = TRUE)
        )
      }
      d
    }
  )
  dplyr::bind_cols(proposals)
}

fill_proposals <- function(proposals, calib_object) {
  default_proposal <- calib_object$state$default_proposal
  missing_cols <- setdiff(names(default_proposal), names(proposals))
  proposals <- merge_proposals(list(
    proposals,
    default_proposal[, missing_cols]
  ))
  proposals <- adjust_proposal_number(proposals, calib_object)

  proposals
}

adjust_proposal_number <- function(proposals, calib_object) {
  n_sims <- calib_object$config$n_sims
  if (nrow(proposals) > n_sims) {
    proposals <- dplyr::slice_sample(proposals, n = n_sims, replace = TRUE)
  } else if (nrow(proposals) < n_sims) {
    missing_proposals <- n_sims - nrow(proposals)
    proposals <- dplyr::bind_rows(
      proposals,
      dplyr::slice_sample(proposals, n = missing_proposals, replace = TRUE)
    )
  }
  proposals
}

get_long_calibrated_proposal <- function(calib_object) {
  calibrated_proposal <- calib_object$calibrated_proposal
  long_calib <- tidyr::pivot_longer(
    calibrated_proposal, cols = dplyr::everything(),
    names_to = "param", values_to = "value"
  )
  long_calib$type <- vapply(calibrated_proposal, get_param_type, "")
  long_calib
}

update_default_proposal <- function(calib_object, job_results) {
  default_proposal <- calib_object$state$default_proposal
  for (i in seq_along(job_results)) {
    result <- job_results[[i]]
    if (!is.null(result)) {
      shared_params <- intersect(names(result), names(default_proposal))
      default_proposal[, shared_params] <- result[, shared_params]
    }
  }
  calib_object$state$default_proposal <- default_proposal
  calib_object
}
