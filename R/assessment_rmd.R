make_wave_rmd <- function(assessments, wave_num) {
  wave <- assessments[[paste0("wave", wave_num)]]
  cat("# Wave", wave_num, "\n\n")
  for (i in seq_along(wave)) {
    make_job_rmd(wave[[i]])
  }
}

make_job_rmd <- function(job_assess) {
  cat("##", job_assess$infos$job_id, "\n\n")

  cat("### Targets and Parameters", "\n\n")
  dplyr::tibble(
    target_name = job_assess$infos$targets,
    target_value = job_assess$infos$targets_val
  ) |> knitr::kable() |> print()

  dplyr::tibble(
    parameter = job_assess$infos$params,
    initial_range = vapply(
      job_assess$infos$params_ranges,
      \(x) paste0(x[1], " - ", x[2]),
      ""
    )
  ) |> knitr::kable() |> print()

  cat("\n\n")

  cat("### Parameter Space and RMSE Evolution", "\n\n")

  make_param_volume_plot(job_assess) |> print()
  make_rmse_plot(job_assess) |> print()
  cat("\n\n")

  cat("### Parameter Spreads", "\n\n")
  for (p in job_assess$infos$params) {
    make_param_spread_plot(job_assess, p) |> print()
    cat("\n\n")
  }

  cat("### Target Errors", "\n\n")
  for (t in job_assess$infos$targets) {
    make_target_err_plot(job_assess, t) |> print()
    cat("\n\n")
  }
  cat("\n\n")

}
