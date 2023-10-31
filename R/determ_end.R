#' @export
determ_end_thresh <- function(thresholds, n_enough) {
  force(thresholds)
  force(n_enough)
  function(calib_object, job, results) {
    # Enough close enough estimates?
    p_ok <- results[, c(job$params, job$targets)]
    for (j in seq_along(job$targets)) {
      values <- p_ok[[ job$targets[j] ]]
      target <- job$targets_val[j]
      thresh <- thresholds[j]
      p_ok <- p_ok[abs(values - target) < thresh, ]
    }

    if (nrow(p_ok) > n_enough) {
      res <- p_ok[, job$params]
      # get the n_tuple where all values are the closest to the median
      best <- dplyr::summarise(
        res,
        dplyr::across(dplyr::everything(), ~ abs(.x - median(.x)))
      )
      best <- which.min(rowSums(best))
      return(res[best, ])
    } else {
      return(NULL)
    }
  }
}

# can be used for 1 or many params I think
# save centers in sideload using `list(centers = newp)`
#' @export
determ_poly_end <- function(threshold, poly_n = 3) {
  force(threshold)
  force(poly_n)
  function(calib_object, job, results) {
    mscale <- function(x, val) (x - mean(val)) / sd(val)
    munscale <- function(x, val) x * sd(val) + mean(val)

    values <- c()
    params <- c()
    targets <- job$targets_val

    for (i in seq_along(job$targets)) {
      values <- c(values, results[[ job$targets[i] ]])
      params <- c(params, results[[ job$params[i] ]])
    }

    complete_rows <- vctrs::vec_detect_complete(values)
    values <- values[complete_rows]
    params <- params[complete_rows]

    s_v <- mscale(values, values)
    s_t <- mscale(targets, values)
    s_p <- mscale(params, params)

    mod <- lm(s_v ~ poly(s_p, poly_n))
    loss_fun <- function(par, t)  abs(predict(mod, data.frame(s_p = par)) - t)
    s_newp <- vapply(
      s_t,
      function(t) optimize(interval = range(s_p), f = loss_fun, t = t)$minimum,
      numeric(1)
    )
    s_newv <- predict(mod, data.frame(s_p = s_newp))
    newp <- munscale(s_newp, params)

    oldp <- swfcalib::load_sideload(calib_object, job)$centers
    swfcalib::save_sideload(calib_object, job, list(centers = newp))

    if (is.null(oldp)) return(NULL)

    s_oldp <- mscale(oldp, params)
    s_oldv <- predict(mod, data.frame(s_p = s_oldp))

    newv <- munscale(s_newv, values)
    oldv <- munscale(s_oldv, values)

    if (all(abs(oldv - newv) < threshold & abs(newv - targets) < threshold)) {
      result <- as.list(newp)
      names(result) <- job$params
      return(dplyr::as_tibble(result))
    } else {
      return(NULL)
    }
  }
}
