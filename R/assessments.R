make_rmse <- function(x, target) sqrt(mean((target - x)^2))
make_mae <- function(x, target) mean(abs(target - x))

get_spread <- function(x) diff(range(x))

make_assessments <- function(calib_object, results) {
  if (nrow(results) == 0) {
    return(list(jobs = list(), raw = dplyr::tibble()))
  }

  assessments <- future.apply::future_lapply(
    get_current_jobs(calib_object),
    make_job_assessments,
    calib_object = calib_object,
    results = results
  )

  assessments <- purrr::list_transpose(assessments, simplify = FALSE)
  out <- list()
  out$raw <- purrr::reduce(assessments$raw, dplyr::left_join, by = ".iteration")
  out$jobs <- assessments$job
  names(out$jobs) <- purrr::flatten_chr(assessments$job_id)

  old_assessments <- load_assessments(calib_object)
  out <- merge_assessments(out, old_assessments)

  out
}

merge_assessments <- function(new, old) {
  out <- list(
    raw = dplyr::bind_rows(old$raw, new$raw),
    jobs = list()
  )

  for (job_name in names(new$jobs)) {
    out$jobs[[job_name]] <- dplyr::bind_rows(
      old$jobs[[job_name]],
      new$jobs[[job_name]]
    )
  }
  out
}

load_assessments <- function(calib_object) {
  out <- list(
    raw = load_assessment_file(get_raw_assessment_path(calib_object)),
    jobs = list()
  )

  job_ids <- vapply(get_current_jobs(calib_object), get_job_id, "")
  for (job_id in job_ids) {
    out$jobs[[job_id]] <- load_assessment_file(
      get_job_assessment_path(calib_object, job_id)
    )
  }
  names(out$jobs) <- job_ids
  out
}

load_assessment_file <- function(file_path) {
  if (fs::file_exists(file_path)) readRDS(file_path) else dplyr::tibble()
}

load_job_assessment <- function(calib_object, job_id) {
  file_path <- get_job_assessment_path(calib_object, job_id)
  if (fs::file_exists(file_path)) readRDS(file_path) else dplyr::tibble()
}

save_assessments <- function(calib_object, assessments) {
  saveRDS(assessments[["raw"]], get_raw_assessment_path(calib_object))
  for (job_id in names(assessments$jobs)) {
    saveRDS(
      assessments$jobs[[job_id]],
      get_job_assessment_path(calib_object, job_id)
    )
  }

  readr::write_csv(assessments[["raw"]], get_csv_assessment_path(calib_object))
}

get_assessments_dir <- function(calib_object) {
  fs::path(get_current_wave_dir(calib_object), "assessments")
}

get_raw_assessment_path <- function(calib_object) {
  fs::path(get_assessments_dir(calib_object), "raw.rds")
}

get_job_assessment_path <- function(calib_object, job_id) {
  fs::path(get_assessments_dir(calib_object), paste0(job_id, ".rds"))
}

get_csv_assessment_path <- function(calib_object) {
  fs::path(get_assessments_dir(calib_object), "raw.csv")
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

  job_assessment <- dplyr::tibble(
    .iteration = current_iteration,
    rmse_mean = mean(iter_rmse),
    rmse_sd = sd(iter_rmse),
    mae_mean = mean(iter_mae),
    mae_sd = sd(iter_mae),
    param_volume = prod(spreads)
  )

  raw_assessment <- dplyr::tibble(.iteration = current_iteration)
  raw_assessment[paste0("spread_", names(spreads))] <- as.list(spreads)
  raw_assessment[paste0("mean_err_", job$targets)] <- lapply(errors, mean)
  raw_assessment[paste0("sd_err_", job$targets)] <- lapply(errors, sd)

  list(
    job_id = get_job_id(job),
    raw = raw_assessment,
    job = job_assessment
  )
}
