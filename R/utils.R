# Getters ----------------------------------------------------------------------
get_root_dir <- function(calib_object) {
  calib_object$config$root_directory
}

get_current_wave <- function(calib_object) {
  calib_object$state$wave
}

get_current_jobs <- function(calib_object) {
  current_wave <- get_current_wave(calib_object)
  calib_object$waves[[current_wave]]
}

get_current_iteration <- function(calib_object) {
  calib_object$state$iteration
}

get_max_iteration <- function(calib_object) {
  calib_object$config$max_iteration
}

get_results_path <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "results.rds")
}

get_current_wave_dir <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  current_wave <- get_current_wave(calib_object)
  fs::path(root_directory, "waves", current_wave)
}

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

get_n_sims <- function(calib_object) {
  calib_object$config$n_sims
}

get_save_path <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "calib_object.rds")
}

get_sim_result_save_dir <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "sim_results")
}

get_sim_result_save_path <- function(calib_object, i) {
  fs::path(get_sim_result_save_dir(calib_object), paste0(i, ".rds"))
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

get_full_results_path <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "full_results.rds")
}

get_sideload_dir <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "sideloads")
}

get_sideload_path <- function(calib_object, job) {
  sideload_directory <- get_sideload_dir(calib_object)
  fs::path(sideload_directory, paste0(get_job_id(job), ".rds"))
}

get_job_id <- function(job) {
  job$id
}

# # Checkers -------------------------------------------------------------------
is_valid_iteration <- function(calib_object) {
  get_current_iteration(calib_object) <= get_max_iteration(calib_object)
}

is_valid_wave <- function(calib_object) {
  get_current_wave(calib_object) <= length(calib_object$waves)
}

is_calibration_complete <- function(calib_object) {
  !is_valid_wave(calib_object)
}

is_wave_done <- function(calib_object) {
  calib_object$state$done
}

# # Input-Output ---------------------------------------------------------------
make_folders <- function(calib_object) {
  dirs <- c(
    get_sim_result_save_dir(calib_object),
    get_sideload_dir(calib_object)
  )
  for (d in dirs) {
    if (!fs::dir_exists(d)) fs::dir_create(d)
  }
  invisible(TRUE)
}

# calib_save_path must be a variable in all SWF steps
load_calib_object <- function(calib_object) {
  save_path <- get_save_path(calib_object)
  if (fs::file_exists(save_path)) {
    calib_object <- readRDS(save_path)
  } else {
    message("No calib_object file found. initializing the state and returning")
    calib_object <- initialize_state(calib_object)
    make_folders(calib_object)
  }
  calib_object
}

load_results <- function(calib_object) {
  results_path <- get_results_path(calib_object)
  if (fs::file_exists(results_path)) {
    readRDS(get_results_path(calib_object))
  } else {
    dplyr::tibble()
  }
}

save_results <- function(calib_object, new_results) {
  old_result <- load_results(calib_object)
  results <- dplyr::bind_rows(old_result, new_results)
  saveRDS(results, get_results_path(calib_object))
  invisible(TRUE)
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

save_calib_object <- function(calib_object) {
  saveRDS(calib_object, get_save_path(calib_object))
}

save_sim_result <- function(calib_object, sim_results, i, proposal) {
  save_path <- get_sim_result_save_path(calib_object, i)
  sim_results[[".wave"]] <- get_current_wave(calib_object)
  sim_results[[".iteration"]] <- get_current_iteration(calib_object)
  sim_results[[".proposal_index"]] <- proposal[[".proposal_index"]]
  saveRDS(sim_results, save_path)
}

load_sim_results <- function(calib_object) {
  sim_result_save_dir <- get_sim_result_save_dir(calib_object)
  sim_result_files <- fs::dir_ls(sim_result_save_dir)
  if (length(sim_result_files) == 0) {
    dplyr::tibble(
      .proposal_index = numeric(0),
      .iteration = numeric(0),
      .wave = numeric(0)
    )
  } else {
    dplyr::bind_rows(future.apply::future_lapply(sim_result_files, readRDS))
  }
}

clear_sim_results <- function(calib_object) {
  sim_result_save_dir <- get_sim_result_save_dir(calib_object)
  sim_result_files <- fs::dir_ls(sim_result_save_dir)
  fs::file_delete(sim_result_files)
  invisible(TRUE)
}

# function called after the calibration is finished
# so wave = max_wave + 1
load_full_results <- function(calib_object) {
  current_wave <- get_current_wave(calib_object) - 1
  wave_results <- lapply(
    seq_len(current_wave),
    function(wave) {
      calib_object <- mutate_calib_state(calib_object, "wave", wave)
      load_results(calib_object)
    }
  )
  dplyr::bind_rows(wave_results)
}

save_full_results <- function(calib_object, full_results) {
  full_results_path <- get_full_results_path(calib_object)
  saveRDS(full_results, full_results_path)
}

#' Save some data to be reused by the calibration process
#' @export
save_sideload <- function(calib_object, job, x) {
  sl_path <- get_sideload_path(calib_object, job)
  saveRDS(x, sl_path)
}

#' Read some data saved to be reused by the calibration process
#' @export
load_sideload <- function(calib_object, job) {
  sl_path <- get_sideload_path(calib_object, job)
  if (!fs::file_exists(sl_path)) {
    return(NULL)
  } else {
    readRDS(sl_path)
  }
}

# # Updaters -------------------------------------------------------------------

increment_iteration <- function(calib_object) {
  current_iteration <- get_current_iteration(calib_object)
  mutate_calib_state(calib_object, "iteration", current_iteration + 1)
}

decrement_iteration <- function(calib_object) {
  current_iteration <- get_current_iteration(calib_object)
  mutate_calib_state(calib_object, "iteration", current_iteration - 1)
}

increment_wave <- function(calib_object) {
  current_wave <- get_current_wave(calib_object)
  calib_object <- mutate_done_status(calib_object, FALSE)
  calib_object <- mutate_calib_state(calib_object, "iteration", 1)
  calib_object <- mutate_calib_state(calib_object, "wave", current_wave + 1)
  make_folders(calib_object)
  calib_object
}

initialize_state <- function(calib_object) {
  calib_object$state <- list()
  calib_object$state$done <- FALSE
  calib_object$state$wave <- 1
  calib_object$state$iteration <- 0
  calib_object$state$default_proposal <- calib_object$config$default_proposal

  calib_object <- initialize_job_ids(calib_object)

  calib_object
}

initialize_job_ids <- function(calib_object) {
  for (wave_i in seq_along(calib_object$waves)) {
    for (job_i in seq_along(calib_object$waves[[wave_i]])) {
      calib_object$waves[[wave_i]][[job_i]]$id <-
        paste0("waves", wave_i, "-job", job_i)
    }
  }
  calib_object
}

mutate_done_status <- function(calib_object, done) {
  mutate_calib_state(calib_object, "done", done)
}

mutate_default_proposal <- function(calib_object, default_proposal) {
  mutate_calib_state(calib_object, "default_proposal", default_proposal)
}

mutate_calib_state <- function(calib_object, item, value) {
  calib_object$state[[item]] <- value
  calib_object
}


# Proposals --------------------------------------------------------------------
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

# # Simulator ------------------------------------------------------------------
run_simulation <- function(calib_object, proposal) {
  calib_object$config$simulator(proposal)
}

# # Results --------------------------------------------------------------------
merge_results <- function(calib_object) {
  proposals <- load_proposals(calib_object)
  sim_results <- load_sim_results(calib_object)
  # if multiple sim per proposal -> keep them all
  dplyr::right_join(
    proposals, sim_results,
    by = c(".proposal_index", ".wave", ".iteration")
  )
}

# # High level functions -------------------------------------------------------
process_sim_results <- function(calib_object) {
  if (get_current_iteration(calib_object) != 0) {
    new_results <- merge_results(calib_object)
    clear_sim_results(calib_object)

    # If less results than expected, discard this iteration
    # (for instance if the cluster stopped during the step 2)
    if (nrow(new_results) < get_n_sims(calib_object)) {
      calib_object <- decrement_iteration(calib_object)
    } else {
      save_results(calib_object, new_results)
    }
  }
  calib_object
}

get_jobs_results <- function(calib_object, results) {
  future.apply::future_lapply(
    get_current_jobs(calib_object),
    function(co, job, res) job$get_result(co, job, res),
    res = results,
    co = calib_object,
    future.seed = TRUE
  )
}

update_done_status <- function(calib_object, job_results) {
  done <- !any(vapply(job_results, is.null, logical(1)))
  mutate_done_status(calib_object, done)
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

wrap_up_calibration <- function(calib_object) {
  save_calib_object(calib_object)
  message("Calibration complete")
  next_step <- slurmworkflow::get_current_workflow_step() + 2
  slurmworkflow::change_next_workflow_step(next_step)
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
