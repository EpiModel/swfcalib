get_proposals_path <- function(calib_object) {
  current_wave_dir <- get_current_wave_dir(calib_object)
  fs::path(current_wave_dir, "proposals.rds")
}

get_default_proposal <- function(calib_object) {
  calib_object$state$default_proposal
}

get_proposal_n <- function(proposals, n) {
  dplyr::filter(proposals, .data$`.proposal_index` == n)
}

load_proposals <- function(calib_object) {
  proposals_path <- get_proposals_path(calib_object)
  if (fs::file_exists(proposals_path)) {
    readRDS(get_proposals_path(calib_object))
  } else {
    dplyr::tibble()
  }
}

save_proposals <- function(calib_object, proposals) {
  saveRDS(proposals, get_proposals_path(calib_object))
  invisible(TRUE)
}

mutate_default_proposal <- function(calib_object, default_proposal) {
  mutate_calib_state(calib_object, "default_proposal", default_proposal)
}

make_proposals <- function(calib_object, results) {
  current_jobs <- get_current_jobs(calib_object)
  if (get_current_iteration(calib_object) == 1) {
    proposals <- lapply(current_jobs, function(job) job$initial_proposals)
  } else {
    proposals <- future.apply::future_lapply(
      current_jobs,
      function(co, job, res) job$make_next_proposals(co, job, res),
      res = results,
      co = calib_object,
      future.seed = TRUE
    )
  }
  proposals <- merge_proposals(proposals)
  proposals <- fill_proposals(proposals, calib_object)
  proposals[[".proposal_index"]] <- seq_len(nrow(proposals))
  proposals[[".wave"]] <- get_current_wave(calib_object)
  proposals[[".iteration"]] <- get_current_iteration(calib_object)
  dplyr::select(
    proposals,
    dplyr::everything(), ".proposal_index", ".wave", ".iteration"
  )
}

merge_proposals <- function(proposals) {
  max_rows <- max(vapply(proposals, nrow, numeric(1)))
  proposals <- future.apply::future_lapply(
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
    },
    future.seed = TRUE
  )
  dplyr::bind_cols(proposals)
}

fill_proposals <- function(proposals, calib_object) {
  default_proposal <- get_default_proposal(calib_object)
  missing_cols <- setdiff(names(default_proposal), names(proposals))
  proposals <- merge_proposals(list(
    proposals,
    default_proposal[, missing_cols]
  ))
  proposals <- adjust_proposal_number(proposals, calib_object)

  proposals
}

adjust_proposal_number <- function(proposals, calib_object) {
  n_sims <- get_n_sims(calib_object)
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

get_calibrated_proposal <- function(calib_object) {
  calib_object$calibrated_proposal
}

store_calibrated_proposal <- function(calib_object) {
  calib_object$calibrated_proposal <- get_default_proposal(calib_object)
  calib_object
}

get_long_calibrated_proposal <- function(calib_object) {
  calibrated_proposal <- get_calibrated_proposal(calib_object)
  long_calib <- tidyr::pivot_longer(
    calibrated_proposal, cols = dplyr::everything(),
    names_to = "param", values_to = "value"
  )
  long_calib$type <- vapply(calibrated_proposal, get_param_type, "")
  long_calib
}

update_default_proposal <- function(calib_object, job_results) {
  default_proposal <- get_default_proposal(calib_object)
  for (i in seq_along(job_results)) {
    result <- job_results[[i]]
    if (!is.null(result)) {
      shared_params <- intersect(names(result), names(default_proposal))
      default_proposal[, shared_params] <- result[, shared_params]
    }
  }
  mutate_default_proposal(calib_object, default_proposal)
}

