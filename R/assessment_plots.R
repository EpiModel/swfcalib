make_rmse_plot <- function(job) {
  ggplot(job$measures, aes(x = iteration, y = rmse_mean)) +
    geom_line() +
    geom_ribbon(
      aes(ymin = rmse_mean - rmse_sd, ymax = rmse_mean + rmse_sd),
      alpha = 0.3
    ) +
    scale_y_log10() +
    theme_light() +
    labs(
      title = paste0("RMSE Evolution for: ", job$infos$job_id),
      x = "Iteration",
      y = "RMSE \n(log10 scale)"
  )
}

make_param_volume_plot <- function(job) {
  ggplot(job$measures, aes(x = iteration, y = param_volume)) +
    geom_line() +
    scale_y_log10() +
    theme_light() +
    labs(
      title =
        paste0("Parameter Space Volume Evolution for: ", job$infos$job_id),
      x = "Iteration",
      y = "Parameter Space Volume \n(log10 scale)"
    )
}

make_param_spread_plot <- function(job, param) {
  d <- job$measures
  d[["y"]] <- d[[paste0("spread__", param)]]
  ggplot(d, aes(x = iteration, y = y)) +
    geom_line() +
    scale_y_log10() +
    theme_light() +
    labs(
      title = paste0("Spread of Parameter: ", param),
      x = "Iteration",
      y = "Spread \n(log10 scale)"
    )
}

make_target_err_plot <- function(job, target) {
  d <- job$measures
  d[["y"]] <- d[[paste0("mean_err__", target)]]
  d[["ys"]] <- d[[paste0("sd_err__", target)]]
  ggplot(d, aes(x = iteration, y = y)) +
    geom_line() +
    geom_ribbon(
      aes(ymin = y - ys, ymax = y + ys),
      alpha = 0.3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_light() +
    labs(
      title = paste0("Mean Error on: ", target),
      x = "Iteration",
      y = "Mean Error"
    )
}

make_job_plots <- function(job) {
  out <- list()
  out$rmse <- make_rmse_plot(job)
  out$volume <- make_param_volume_plot(job)
  out$params <- lapply(job$infos$params, make_param_spread_plot, job = job)
  names(out$params) <- job$infos$params
  out$targets <- lapply(job$infos$targets, make_target_err_plot, job = job)
  names(out$targets) <- job$infos$targets
  out
}

make_wave_plots <- function(wave) {
  out <- lapply(wave, make_job_plots)
  names(out) <- vapply(wave, function(x) x$infos$job_id, character(1))
  out
}

make_assessments_plots <- function(assessments) {
  out <- lapply(assessments, make_wave_plots)
  names(out) <- names(assessments)
  out
}
