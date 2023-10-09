make_rmse <- function(x, target) sqrt(mean((target - x)^2))
make_mae <- function(x, target) mean(abs(target - x))

get_spread <- function(x) diff(range(x))

make_assessments <- function(calib_object, results) {
  if (nrow(results) == 0) {
    return(list(jobs = list(), raw = dplyr::tibble()))
  }

  ### --------------------------------------------------------------------------
  ### this only makes the last iter
  ### I need to bind it to the previous ones
  ###
  ### - get old ones
  ### - merge
  ### - return
  ### --------------------------------------------------------------------------
  assessements <- future.apply::future_lapply(
    get_current_jobs(calib_object),
    make_job_assessments,
    calib_object = calib_object,
    results = results
  )

  assessements <- purrr::list_transpose(assessements)
  assessements$raw <- dplyr::bind_cols(assessements$raw)
  assessements
}

save_assessements <- function(calib_object, assessements) {
  saveRDS(assessements[["raw"]], get_raw_assessement_path(calib_object))
  for (i in seq_along(assessements$job_id)) {
    saveRDS(
      assessements$jobs[[i]],
      get_job_assessement_path(calib_object, assessements$jobs_id[[i]])
    )
  }
}

get_assessements_dir <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "assessements")
}

get_raw_assessement_path <- function(calib_object) {
  fs::path(get_assessements_dir(calib_object), "raw.rds")
}

get_job_assessement_path <- function(calib_object, job_id) {
  fs::path(get_assessements_dir(calib_object), paste0(job_id, ".rds"))
}


make_job_assessments <- function(calib_object, job, results) {
  current_iteration <- get_current_iteration(calib_object)
  d <- results |>
    dplyr::filter(.data$.iteration == current_iteration)

  iter_rmse <- apply(d[job$targets], 1, make_rmse, target = job$targets_val)
  iter_mae <- apply(d[job$targets], 1, make_mae, target = job$targets_val)

  errors <- Map(
    function(x, target) target - x,
    d[job$targets],
    job$targets_val
  )

  spreads <- vapply(d[job$params], get_spread, numeric(1))

  job_assessement <- dplyr::tibble(
    .iteration = current_iteration,
    rmse_mean = mean(iter_rmse),
    rmse_sd = sd(iter_rmse),
    mae_mean = mean(iter_mae),
    mae_sd = sd(iter_mae),
    param_volume = prod(spreads)
  )

  raw_assessement <- dplyr::tibble(.iteration = current_iteration)
  raw_assessement[paste0("spread_", names(spreads))] <- as.list(spreads)
  raw_assessement[paste0("mean_err_", job$targets)] <- lapply(errors, mean)
  raw_assessement[paste0("sd_err_", job$targets)] <- lapply(errors, sd)

  list(
    job_id = get_job_id(job),
    raw = raw_assessement,
    job = job_assessement
  )
}

#
