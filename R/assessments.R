update_assessments <- function(calib_object, results) {
  if (nrow(results) == 0) {
    save_assessments(calib_object, list())
    return(invisible(calib_object))
  }

  out <- load_assessments(calib_object)
  cur_wave <- paste0("wave", get_current_wave(calib_object))

  assessments <- future.apply::future_lapply(
    get_current_jobs(calib_object),
    make_job_assessment,
    calib_object = calib_object,
    results = results
  )

  out[[cur_wave]] <- merge_wave_assements(assessments, out[[cur_wave]])
  save_assessments(calib_object, out)
  invisible(calib_object)
}

merge_wave_assements <- function(new, old) {
  old <- if (is.null(old)) list() else old
  for (nme in names(new)) {
    new[[nme]] <- merge_job_assessment(new[[nme]], old[[nme]])
  }
  new
}

merge_job_assessment <- function(new, old) {
  list(
    infos = new$infos,
    measures = dplyr::bind_rows(old$measures, new$measures)
  )
}

save_assessments <- function(calib_object, assessments) {
  saveRDS(assessments, get_assessments_path(calib_object))
}

get_assessments_path <- function(calib_object) {
  fs::path(get_root_dir(calib_object), "assessments.rds")
}

load_assessments <- function(calib_object) {
  f_path <- get_assessments_path(calib_object)
  if (fs::file_exists(f_path)) readRDS(f_path) else list()
}

make_job_assessment <- function(calib_object, job, results) {
  out <- list()

  out$infos <- job[c("targets", "targets_val", "params")]
  out$infos$job_id <- get_job_id(job)
  out$infos$params_ranges <- lapply(job$initial_proposals, range)

  current_iteration <- get_current_iteration(calib_object)
  d <- dplyr::filter(results, .data$.iteration == current_iteration)

  make_rmse <- function(x, target) sqrt(mean((target - x)^2))
  iter_rmse <- apply(d[job$targets], 1, make_rmse, target = job$targets_val)

  get_spread <- function(x) diff(range(x))
  spreads <- vapply(d[job$params], get_spread, numeric(1))

  out$measures <- dplyr::tibble(
    iteration = current_iteration,
    rmse_mean = mean(iter_rmse),
    rmse_sd = sd(iter_rmse),
    param_volume = prod(spreads)
  )

  errors <- Map(function(x, target) target - x, d[job$targets], job$targets_val)

  out$measures[paste0("spread__", names(spreads))] <- as.list(spreads)
  out$measures[paste0("mean_err__", job$targets)] <- lapply(errors, mean)
  out$measures[paste0("sd_err__", job$targets)] <- lapply(errors, sd)

  out
}
