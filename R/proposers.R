#' @export
make_proposer_se_range <- function(n_new, retain_prop = 0.2) {
  force(n_new)
  force(retain_prop)
  function(calib_object, job, results) {
    p_ranges <- list()
    values <- results[job$targets]
    params <- results[job$params]

    params[[".SE_score"]] <- 0
    for (i in seq_along(job$targets)) {
      t <- job$targets_val[i]
      vs <- values[[i]]
      params[[".SE_score"]] <- params[[".SE_score"]] + (vs - t)^2
    }

    params <- dplyr::arrange(params, .data$.SE_score)
    params <- dplyr::select(params, - .data$.SE_score)
    params <- head(params, ceiling(nrow(params) * retain_prop))

    for (i in seq_along(job$targets)) {
      p_ranges[[i]] <- range(params[[i]])
    }

    proposals <- lhs::randomLHS(n_new, length(job$params))
    for (i in seq_along(job$params)) {
      spread <- p_ranges[[i]][2] - p_ranges[[i]][1]
      rmin <- p_ranges[[i]][1]
      proposals[, i] <- proposals[, i] * spread + rmin
    }
    colnames(proposals) <- job$params
    dplyr::as_tibble(proposals)
  }
}

#' Shrink the range by a shrink factor (default 2)
#'
#' If centers are provided in a sideload, use that
#'
#' @export
make_shrink_proposer <- function(n_new, shrink = 2) {
  force(n_new)
  force(shrink)
  function(calib_object, job, results) {
    centers <- swfcalib::load_sideload(calib_object, job)$centers
    if (is.null(centers)) {
      stop("No centers were provided for shrinkage, abort!")
    }

    outs <- list()
    for (i in seq_along(job$params)) {
      tar_range <- range(
        results[[job$params[i]]][
          results[[".iteration"]] == max(results[[".iteration"]])
        ]
      )
      spread <- (tar_range[2] - tar_range[1]) / shrink / 2

      proposals <- seq(
        max(centers[i] - spread, tar_range[1]),
        min(centers[i] + spread, tar_range[2]),
        length.out = n_new
      )

      proposals <- sample(proposals)

      out <- list(proposals)
      names(out) <- job$params[i]
      outs[[i]] <- dplyr::as_tibble(out)
    }
    dplyr::bind_cols(outs)
  }
}
