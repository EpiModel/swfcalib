# Process the simulations, save the results and clear the individual simulation
# results.
# Discard the iteration if some results are missing
process_sim_results <- function(calib_object) {
  if (get_current_iteration(calib_object) != 0) {
    new_results <- merge_sim_results(calib_object)
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

merge_sim_results <- function(calib_object) {
  proposals <- load_proposals(calib_object)
  sim_results <- load_sim_results(calib_object)
  # if multiple sim per proposal -> keep them all
  dplyr::right_join(
    proposals, sim_results,
    by = c(".proposal_index", ".wave", ".iteration")
  )
}

save_results <- function(calib_object, new_results) {
  old_result <- load_results(calib_object)
  results <- dplyr::bind_rows(old_result, new_results)
  saveRDS(results, get_results_path(calib_object))
  invisible(TRUE)
}

load_results <- function(calib_object) {
  results_path <- get_results_path(calib_object)
  if (fs::file_exists(results_path)) {
    readRDS(get_results_path(calib_object))
  } else {
    dplyr::tibble()
  }
}

get_results_path <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "results.rds")
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

get_sim_result_save_dir <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "sim_results")
}

get_sim_result_save_path <- function(calib_object, i) {
  fs::path(get_sim_result_save_dir(calib_object), paste0(i, ".rds"))
}

# `job$job_results` is a function that return NULL if the calibration job is not
# finished and a 1-row `data.frame` of calibrated parameters otherwise.
get_jobs_results <- function(calib_object, results) {
  future.apply::future_lapply(
    get_current_jobs(calib_object),
    function(co, job, res) job$get_result(co, job, res),
    res = results,
    co = calib_object,
    future.seed = TRUE
  )
}

# Full results: concatenation of all waves results
make_full_results <- function(calib_object) {
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

get_full_results_path <- function(calib_object) {
  root_directory <- get_root_dir(calib_object)
  fs::path(root_directory, "full_results.rds")
}


