make_rmse_plot <- function(job_assess) {
  d <- job_assess$measures
  ggplot2::ggplot(d, ggplot2::aes(x = .data$iteration, y = .data$rmse_mean)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = rmse_mean - rmse_sd, ymax = rmse_mean + rmse_sd),
      alpha = 0.3
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = paste0("RMSE Evolution for: ", job_assess$infos$job_id),
      x = "Iteration",
      y = "RMSE \n(log10 scale)"
  )
}

make_param_volume_plot <- function(job_assess) {
  d <- job_assess$measures
  ggplot2::ggplot(d, ggplot2::aes(x = .data$iteration,
                                  y = .data$param_volume)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_light() +
    ggplot2::labs(
      title =
        paste0("Parameter Space Volume Evolution for: ",
               job_assess$infos$job_id),
      x = "Iteration",
      y = "Parameter Space Volume \n(log10 scale)"
    )
}

make_param_spread_plot <- function(job_assess, param) {
  d <- job_assess$measures
  d[["y"]] <- d[[paste0("spread__", param)]]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$iteration, y = .data$y)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = paste0("Spread of Parameter: ", param),
      x = "Iteration",
      y = "Spread \n(log10 scale)"
    )
}

make_target_err_plot <- function(job_assess, target) {
  d <- job_assess$measures
  d[["y"]] <- d[[paste0("mean_err__", target)]]
  d[["ys"]] <- d[[paste0("sd_err__", target)]]
  d[["ymin"]] <- d[[paste0("min_err__", target)]]
  d[["ymax"]] <- d[[paste0("max_err__", target)]]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$iteration, y = .data$y)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$y - .data$ys, ymax = .data$y + .data$ys),
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      alpha = 0.1
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = paste0("Mean Error on: ", target),
      x = "Iteration",
      y = "Error \n(mean + sd + range)"
    )
}

make_job_plots <- function(job_assess) {
  out <- list()
  infos <- job_assess$infos
  out$rmse <- make_rmse_plot(job_assess)
  out$volume <- make_param_volume_plot(job_assess)
  out$params <- lapply(infos$params, make_param_spread_plot, job = job_assess)
  names(out$params) <- infos$params
  out$targets <- lapply(infos$targets, make_target_err_plot, job = job_assess)
  names(out$targets) <- infos$targets
  out
}

make_wave_plots <- function(wave_assess) {
  out <- lapply(wave_assess, make_job_plots)
  names(out) <- vapply(wave_assess, function(x) x$infos$job_id, character(1))
  out
}

make_assessments_plots <- function(assessments) {
  out <- lapply(assessments, make_wave_plots)
  names(out) <- names(assessments)
  out
}
