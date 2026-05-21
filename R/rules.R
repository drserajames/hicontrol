#' Detect x-of-y points beyond a threshold (zone-based rule)
#'
#' Flags any window of \code{y} consecutive points where at least \code{x}
#' fall strictly beyond \code{centre +/- z * threshold}.
#'
#' @param dat Numeric vector of observations.
#' @param centre Central line value.
#' @param threshold Scale of one standard deviation unit.
#' @param x Minimum number of points that must exceed the zone boundary.
#' @param y Window length.
#' @param z Zone boundary in multiples of \code{threshold}.
#'
#' @return Integer vector of start indices of flagged windows, or \code{NULL}.
#' @examples
#' dat <- c(1, 2, 1, 2, 9, 8, 1, 2)
#' rule_xyz(dat, centre = 4, threshold = 1, x = 1, y = 1, z = 3)
#' @export
rule_xyz <- function(dat, centre, threshold, x, y, z) {

  if (length(dat) >= y) {
    upp <- centre + z * threshold
    low <- centre - z * threshold

    c_upp <- c(0, cumsum(dat > upp))
    rsum_upp <- (c_upp[(y + 1):length(c_upp)] - c_upp[1:(length(c_upp) - y)]) / y

    c_low <- c(0, cumsum(dat < low))
    rsum_low <- (c_low[(y + 1):length(c_low)] - c_low[1:(length(c_low) - y)]) / y

    ind <- c(which(rsum_upp >= x / y), which(rsum_low >= x / y))

    return(ind)
  }
}


#' Detect monotone trends
#'
#' Flags runs of \code{n} or more consecutive points that are strictly
#' increasing or strictly decreasing.
#'
#' @param dat Numeric vector of observations.
#' @param n Minimum trend length (number of points).
#'
#' @return A 3-row matrix with columns for each flagged run: row 1 is the
#'   start index, row 2 is the run length, row 3 is direction (+1 up, -1
#'   down). Returns \code{NULL} when no runs are found.
#' @examples
#' dat <- c(9, 7, 5, 3, 1, 4, 7)
#' rule_trend(dat, n = 4)
#' @export
rule_trend <- function(dat, n) {

  if (length(dat) >= n) {
    len  <- length(dat)
    diff <- dat[1:(len - 1)] - dat[2:len]

    run_length_dec  <- rle(diff > 0)
    run_n_dec       <- which(run_length_dec$lengths >= (n - 1) & run_length_dec$values == TRUE)
    ind_dec         <- rep(NA, length(run_n_dec))
    if (length(run_n_dec) > 0) {
      for (i in seq_along(run_n_dec)) {
        if (run_n_dec[1] == 1) {
          ind_dec[1] <- 1
        } else {
          ind_dec[i] <- sum(run_length_dec$lengths[1:(run_n_dec[i] - 1)]) + 1
        }
      }
    }

    run_length_inc <- rle(diff < 0)
    run_n_inc      <- which(run_length_inc$lengths >= (n - 1) & run_length_inc$values == TRUE)
    ind_inc        <- rep(NA, length(run_n_inc))
    if (length(run_n_inc) > 0) {
      for (i in seq_along(run_n_inc)) {
        if (run_n_inc[1] == 1) {
          ind_inc[1] <- 1
        } else {
          ind_inc[i] <- sum(run_length_inc$lengths[1:(run_n_inc[i] - 1)]) + 1
        }
      }
    }

    if (length(ind_dec) > 0) {
      if (length(ind_inc) > 0) {
        ind <- rbind(
          c(ind_dec, ind_inc),
          c(run_length_dec$lengths[run_n_dec] + 1, run_length_inc$lengths[run_n_inc] + 1),
          c(rep(-1, length(run_n_dec)), rep(1, length(run_n_inc)))
        )
        return(ind)
      } else {
        ind <- rbind(ind_dec, run_length_dec$lengths[run_n_dec] + 1, rep(-1, length(run_n_dec)))
        return(ind)
      }
    } else {
      if (length(ind_inc) > 0) {
        ind <- rbind(ind_inc, run_length_inc$lengths[run_n_inc] + 1, rep(1, length(run_n_inc)))
        return(ind)
      }
    }
  }
}


#' Detect runs with no points in zone C (mixture rule)
#'
#' Flags runs of \code{n} or more consecutive points that all fall strictly
#' outside \code{centre +/- threshold} (i.e. none in zone C).
#'
#' @param dat Numeric vector of observations.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n Minimum run length to flag.
#'
#' @return A 2-row matrix: row 1 start indices, row 2 run lengths. \code{NULL}
#'   when nothing is flagged.
#' @examples
#' dat <- c(3, 4, 3, 4, 3, 4, 0)
#' rule_noC(dat, centre = 0, threshold = 1, n = 6)
#' @export
rule_noC <- function(dat, centre, threshold, n) {

  if (length(dat) >= n) {
    run_length <- rle(abs(dat - centre) > threshold)
    run_n      <- which(run_length$lengths >= n & run_length$values == TRUE)
    ind        <- rep(NA, length(run_n))
    if (length(run_n) > 0) {
      for (i in seq_along(run_n)) {
        if (i == 1 && run_n[i] == 1) {
          ind[1] <- 1
        } else {
          ind[i] <- sum(run_length$lengths[1:(run_n[i] - 1)]) + 1
        }
      }
      ind <- rbind(ind, run_length$lengths[run_n])
      return(ind)
    }
  }
}


#' Detect runs of points confined to zone C (stratification rule)
#'
#' Flags runs of \code{n} or more consecutive points that all fall within
#' \code{centre +/- threshold}.
#'
#' @param dat Numeric vector of observations.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n Minimum run length to flag.
#'
#' @return A 2-row matrix: row 1 start indices, row 2 run lengths. \code{NULL}
#'   when nothing is flagged.
#' @examples
#' dat <- c(0.5, -0.5, 0.3, -0.3, 0.5, 5)
#' rule_onlyC(dat, centre = 0, threshold = 1, n = 5)
#' @export
rule_onlyC <- function(dat, centre, threshold, n) {

  if (length(dat) >= n) {
    run_length <- rle(abs(dat - centre) > threshold)
    run_n      <- which(run_length$lengths >= n & run_length$values == FALSE)
    ind        <- rep(NA, length(run_n))
    if (length(run_n) > 0) {
      for (i in seq_along(run_n)) {
        if (i == 1 && run_n[i] == 1) {
          ind[1] <- 1
        } else {
          ind[i] <- sum(run_length$lengths[1:(run_n[i] - 1)]) + 1
        }
      }
      ind <- rbind(ind, run_length$lengths[run_n])
      return(ind)
    }
  }
}


#' Detect alternating (zig-zag) patterns
#'
#' Flags runs of \code{n} or more consecutive points that alternate
#' up and down (over-control rule).
#'
#' @param dat Numeric vector of observations.
#' @param n Minimum alternating run length to flag.
#'
#' @return A 2-row matrix: row 1 start indices, row 2 run lengths. \code{NULL}
#'   when nothing is flagged.
#' @examples
#' dat <- c(1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1)
#' rule_alt(dat, n = 10)
#' @export
rule_alt <- function(dat, n) {
  if (length(dat) >= n) {
    len      <- length(dat)
    diff     <- dat[1:(len - 1)] - dat[2:len]
    diff_sign <- sign(diff)

    c_ds   <- c(0, cumsum(diff_sign))
    rsum_ds <- (c_ds[(2 + 1):length(c_ds)] - c_ds[1:(length(c_ds) - 2)]) / 2

    run_length <- rle(rsum_ds)
    run_n      <- which(run_length$lengths >= (n - 2) & run_length$values == 0)
    ind        <- rep(NA, length(run_n))
    if (length(run_n) > 0) {
      for (i in seq_along(run_n)) {
        if (i == 1 && run_n[i] == 1) {
          ind[1] <- 1
        } else {
          ind[i] <- sum(run_length$lengths[1:(run_n[i] - 1)]) + 1
        }
      }
      match_ind <- na.omit(match(which(diff != 0), ind))
      ind <- rbind(ind[match_ind], run_length$lengths[run_n[match_ind]] + 2)
      return(ind)
    }
  }
}


#' Detect runs of identical consecutive values
#'
#' Flags runs of \code{n} or more consecutive points with no change between
#' successive values (useful for interval-censored data that becomes stuck).
#'
#' @param dat Numeric vector of observations.
#' @param n Minimum run length to flag.
#'
#' @return A 2-row matrix: row 1 start indices, row 2 run lengths. \code{NULL}
#'   when nothing is flagged.
#' @examples
#' dat <- c(1, 2, 2, 2, 2, 2, 3)
#' rule_nodiff(dat, n = 5)
#' @export
rule_nodiff <- function(dat, n) {

  if (length(dat) >= n) {
    len        <- length(dat)
    diff       <- dat[1:(len - 1)] - dat[2:len]
    run_length <- rle(diff)
    run_n      <- which(run_length$lengths >= (n - 1) & run_length$values == 0)
    ind        <- rep(NA, length(run_n))
    if (length(run_n) > 0) {
      for (i in seq_along(run_n)) {
        if (i == 1 && run_n[i] == 1) {
          ind[1] <- 1
        } else {
          ind[i] <- sum(run_length$lengths[1:(run_n[i] - 1)]) + 1
        }
      }
      ind <- rbind(ind, run_length$lengths[run_n] + 1)
      return(ind)
    }
  }
}


#' Detect large single-step differences
#'
#' Flags each pair of consecutive points where the absolute difference is at
#' least \code{d}. Particularly relevant for HI data where a titre jump of
#' \eqn{\ge d} log2 units is biologically meaningful.
#'
#' @param dat Numeric vector of observations.
#' @param d Minimum absolute difference to flag.
#'
#' @return Integer vector of indices \code{i} where \code{|dat[i] - dat[i+1]| >= d}.
#' @examples
#' dat <- c(1, 2, 5, 6, 7)
#' rule_diff(dat, d = 3)
#' @export
rule_diff <- function(dat, d) {
  len  <- length(dat)
  diff <- dat[1:(len - 1)] - dat[2:len]
  ind  <- which(abs(diff) >= d)
  return(ind)
}


#' Detect runs of points at or outside zone C boundary (relaxed mixture rule)
#'
#' Like \code{rule_noC} but uses \code{>=} rather than \code{>}, so points
#' exactly on the \code{centre +/- threshold} boundary are included.
#'
#' @param dat Numeric vector of observations.
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#' @param n Minimum run length to flag.
#'
#' @return A 2-row matrix: row 1 start indices, row 2 run lengths. \code{NULL}
#'   when nothing is flagged.
#' @examples
#' dat <- c(3, 1, 3, 1, 3, 1, 0)
#' rule_noCrelax(dat, centre = 0, threshold = 1, n = 6)
#' @export
rule_noCrelax <- function(dat, centre, threshold, n) {

  if (length(dat) >= n) {
    run_length <- rle(abs(dat - centre) >= threshold)
    run_n      <- which(run_length$lengths >= n & run_length$values == TRUE)
    ind        <- rep(NA, length(run_n))
    if (length(run_n) > 0) {
      for (i in seq_along(run_n)) {
        if (i == 1 && run_n[i] == 1) {
          ind[1] <- 1
        } else {
          ind[i] <- sum(run_length$lengths[1:(run_n[i] - 1)]) + 1
        }
      }
      ind <- rbind(ind, run_length$lengths[run_n])
      return(ind)
    }
  }
}
