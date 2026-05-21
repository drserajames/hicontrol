#' Plot a Nelson-style control chart with rule violations highlighted
#'
#' Draws the data as a connected series, adds horizontal lines for the
#' centre and ±1/2/3 SD boundaries, then overlays coloured points and
#' line segments for each flagged rule violation.
#'
#' @param dat Numeric vector of observations.
#' @param results List of rule results as returned by \code{\link{hi_rules}}.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n_length Integer vector of length 8 giving the window length for
#'   each rule (used for drawing violation segments).
#'
#' @return Called for its side effect (a plot). Returns \code{NULL} invisibly.
#' @examples
#' dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
#' res <- hi_rules(dat, centre = 4, threshold = 1)
#' plot_nelson(dat, res[[1]], centre = 4, threshold = 1,
#'             n_length = c(1, 3, 8, 9, 25, 10, 4, 2))
#' @export
plot_nelson <- function(dat, results, centre, threshold, n_length) {
  plot(dat, type = "o", pch = 16, cex = 0.6, lwd = 0.8)

  abline(h = centre)
  abline(h = c(centre + threshold,  centre - threshold),  lty = 5)
  abline(h = c(centre + 2*threshold, centre - 2*threshold), lty = 2)
  abline(h = c(centre + 3*threshold, centre - 3*threshold), lty = 3)

  cols    <- c("red", "orange", "yellow", "tan", "blue", "magenta", "pink", "purple")
  cexs    <- c(20:13) / 20
  ltys    <- c(NA, "solid", "longdash", "dotted", "F4", "dashed", "dotdash", "twodash")
  threshs <- c(3, 2, 1, 1)

  # Rules 1–3: vector results (start indices); zone-based coloured points + window lines
  for (i in 3:1) {
    if (length(results[[i]]) > 0) {
      points((seq_along(dat))[abs(dat - centre) > threshs[i]],
             dat[abs(dat - centre) > threshs[i]],
             pch = 16, col = cols[i], cex = cexs[i])
      if (i > 1) {
        for (j in seq_along(results[[i]])) {
          lines(1:n_length[i] + results[[i]][j] - 1,
                dat[1:n_length[i] + results[[i]][j] - 1],
                col = cols[i])
        }
      }
    }
  }

  # Rule 4: 2-row matrix result (row 1 = start, row 2 = length);
  # zone-based points + run lines
  if (!is.null(results[[4]])) {
    points((seq_along(dat))[abs(dat - centre) > threshs[4]],
           dat[abs(dat - centre) > threshs[4]],
           pch = 16, col = cols[4], cex = cexs[4])
    for (j in 1:ncol(results[[4]])) {
      lines(1:results[[4]][2, j] + results[[4]][1, j] - 1,
            dat[1:results[[4]][2, j] + results[[4]][1, j] - 1],
            col = cols[4])
    }
  }

  # Rules 5–7: matrix results (row 1 = start, row 2 = length); run points + lines
  for (i in 5:7) {
    if (!is.null(results[[i]])) {
      for (j in 1:ncol(results[[i]])) {
        points(1:results[[i]][2, j] + results[[i]][1, j] - 1,
               dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
               pch = 16, col = cols[i], cex = cexs[i])
        lines(1:results[[i]][2, j] + results[[i]][1, j] - 1,
              dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
              col = cols[i])
      }
    }
  }

  # Rule 8: vector result (step start indices); 2-point lines
  if (length(results[[8]]) > 0) {
    for (j in seq_along(results[[8]])) {
      lines(1:n_length[8] + results[[8]][j] - 1,
            dat[1:n_length[8] + results[[8]][j] - 1],
            col = cols[8])
    }
  }

  # Overplot with line types for disambiguation
  # Rules 2–3: vector results
  for (i in 2:3) {
    if (length(results[[i]]) > 0) {
      for (j in seq_along(results[[i]])) {
        lines(1:n_length[i] + results[[i]][j] - 1,
              dat[1:n_length[i] + results[[i]][j] - 1],
              col = cols[i], lty = ltys[i])
      }
    }
  }

  # Rule 4 overplot
  if (!is.null(results[[4]])) {
    for (j in 1:ncol(results[[4]])) {
      lines(1:results[[4]][2, j] + results[[4]][1, j] - 1,
            dat[1:results[[4]][2, j] + results[[4]][1, j] - 1],
            col = cols[4], lty = ltys[4])
    }
  }

  # Rules 5–7 overplot
  for (i in 5:7) {
    if (!is.null(results[[i]])) {
      for (j in 1:ncol(results[[i]])) {
        lines(1:results[[i]][2, j] + results[[i]][1, j] - 1,
              dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
              col = cols[i], lty = ltys[i])
      }
    }
  }

  # Rule 8 overplot
  if (length(results[[8]]) > 0) {
    for (j in seq_along(results[[8]])) {
      lines(1:n_length[8] + results[[8]][j] - 1,
            dat[1:n_length[8] + results[[8]][j] - 1],
            col = cols[8], lty = ltys[8])
    }
  }

  invisible(NULL)
}


#' Plot an HI control chart with rule violations highlighted
#'
#' Symmetric y-axis version of the control chart, scaled so the centre line
#' sits at the midpoint. Suitable for a single antigen/serum pair.
#'
#' @param dat Numeric vector of observations (log2 titre scale).
#' @param results List of rule results as returned by \code{\link{hi_rules}}.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n_length Integer vector of length 8: window length per rule.
#' @param ... Additional arguments passed to \code{plot()}.
#'
#' @return Called for its side effect (a plot). Returns \code{NULL} invisibly.
#' @examples
#' dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
#' res <- hi_rules(dat, centre = 4, threshold = 1)
#' plot_hi(dat, res[[1]], centre = 4, threshold = 1,
#'         n_length = c(1, 3, 8, 9, 25, 10, 4, 2))
#' @export
plot_hi <- function(dat, results, centre, threshold, n_length, ...) {
  max_y <- max(abs(dat - centre), na.rm = TRUE)
  plot(dat, type = "o", pch = 16, cex = 0.6, lwd = 0.8,
       ylim = c(max_y, -max_y) + centre, ...)

  abline(h = centre)
  abline(h = c(centre + threshold,  centre - threshold),  lty = 5)
  abline(h = c(centre + 2*threshold, centre - 2*threshold), lty = 2)
  abline(h = c(centre + 3*threshold, centre - 3*threshold), lty = 3)

  cols    <- c("red", "orange", "yellow", "tan", "seagreen", "purple", "blue", "magenta")
  cexs    <- c(20:13) / 20
  threshs <- c(3, 2, 1, 1)

  for (i in c(8, 3:1)) {
    if (length(results[[i]]) > 0) {
      points((seq_along(dat))[abs(dat - centre) >= threshs[i]],
             dat[abs(dat - centre) >= threshs[i]],
             pch = 16, col = cols[i], cex = cexs[i])
      if (i > 1) {
        for (j in seq_along(results[[i]])) {
          lines(1:n_length[i] + results[[i]][j] - 1,
                dat[1:n_length[i] + results[[i]][j] - 1],
                col = cols[i])
        }
      }
    }
  }

  if (length(results) > 3) {
    for (i in 4:7) {
      if (length(results[[i]]) > 0) {
        for (j in 1:ncol(results[[i]])) {
          points(1:results[[i]][2, j] + results[[i]][1, j] - 1,
                 dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
                 pch = 16, col = cols[i], cex = cexs[i])
          lines(1:results[[i]][2, j] + results[[i]][1, j] - 1,
                dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
                col = cols[i])
        }
      }
    }
  }

  invisible(NULL)
}


#' Plot a compact HI control chart for use in multi-panel layouts
#'
#' Draws a minimal chart with a fixed y-axis range suitable for tiling many
#' antigen/serum combinations. Flags a pink background when any rule fires.
#'
#' @param dat Numeric vector of observations (log2 titre scale).
#' @param results List of rule results as returned by \code{\link{hi_rules2}}.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n_length Integer vector of length 8: window length per rule.
#' @param ttl_len Total number of time points (used to scale point and line
#'   sizes for readability at small panel sizes).
#' @param ... Additional arguments passed to \code{plot()}.
#'
#' @return Called for its side effect (a plot). Returns \code{NULL} invisibly.
#' @examples
#' dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
#' res <- hi_rules2(dat, centre = 4, threshold = 1)
#' plot_hi2(dat, res[[1]], centre = 4, threshold = 1,
#'          n_length = c(1, 3, 8, 9, 25, 10, 4, 2), ttl_len = length(dat))
#' @export
plot_hi2 <- function(dat, results, centre, threshold, n_length, ttl_len, ...) {
  plot(NA, axes = FALSE, frame.plot = TRUE,
       ylim = c(-1.5, 10), xlim = c(0.5, ttl_len + 0.5), xaxs = "i")

  if (length(unlist(results)) > 0) {
    rect(0.7, -1.5 - 11.5 * 0.2, ttl_len + 0.48, 10 + 11.5 * 0.2,
         col = "mistyrose", border = FALSE)
  }

  abline(h = -1:10, col = "grey85", lwd = 0.2)
  abline(h = centre,  col = "grey50", lwd = 0.2)

  points(dat, pch = 16, cex = 15 / ttl_len)
  lines(dat, lwd = 15 / ttl_len)

  cols <- c("red", "orange", "yellow", "tan", "seagreen", "purple", "blue", "magenta")
  ltys <- c(NA, "solid", "longdash", "dotted", "F4", "dashed", "dotdash", "twodash")

  for (i in 1) {
    points((seq_along(dat))[abs(dat - centre) >= 3],
           dat[abs(dat - centre) >= 3],
           pch = 16, col = cols[i], cex = 20 / ttl_len)
  }

  for (i in c(8, 3:1)) {
    if (length(results[[i]]) > 0 && i > 1) {
      for (j in seq_along(results[[i]])) {
        points(1:n_length[i] + results[[i]][j] - 1,
               dat[1:n_length[i] + results[[i]][j] - 1],
               col = cols[i], pch = 16, cex = 15 / ttl_len)
        lines(1:n_length[i] + results[[i]][j] - 1,
              dat[1:n_length[i] + results[[i]][j] - 1],
              col = cols[i], lwd = 15 / ttl_len)
      }
    }
  }

  if (length(results) > 3) {
    for (i in 4:7) {
      if (length(results[[i]]) > 0) {
        for (j in 1:ncol(results[[i]])) {
          points(1:results[[i]][2, j] + results[[i]][1, j] - 1,
                 dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
                 pch = 16, col = cols[i], cex = 15 / ttl_len)
          lines(1:results[[i]][2, j] + results[[i]][1, j] - 1,
                dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
                col = cols[i], lwd = 15 / ttl_len)
        }
      }
    }
  }

  # overplot with line types for disambiguation
  for (i in c(8, 3:1)) {
    if (length(results[[i]]) > 0 && i > 1) {
      for (j in seq_along(results[[i]])) {
        points(1:n_length[i] + results[[i]][j] - 1,
               dat[1:n_length[i] + results[[i]][j] - 1],
               col = cols[i], pch = 16, cex = 15 / ttl_len)
        lines(1:n_length[i] + results[[i]][j] - 1,
              dat[1:n_length[i] + results[[i]][j] - 1],
              col = cols[i], lwd = 15 / ttl_len, lty = ltys[i])
      }
    }
  }
  if (length(results) > 3) {
    for (i in 4:7) {
      if (length(results[[i]]) > 0) {
        for (j in 1:ncol(results[[i]])) {
          lines(1:results[[i]][2, j] + results[[i]][1, j] - 1,
                dat[1:results[[i]][2, j] + results[[i]][1, j] - 1],
                col = cols[i], lwd = 15 / ttl_len, lty = ltys[i])
        }
      }
    }
  }

  invisible(NULL)
}
