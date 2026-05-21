#' Apply all eight HI control rules and return false-positive rates
#'
#' Runs the full set of eight control chart rules adapted for HI titre data
#' and returns both the raw flagged-index results and the per-rule
#' false-positive rate (proportion of observations flagged).
#'
#' The eight rules are:
#' \enumerate{
#'   \item 1 point >= 3 SD from centre
#'   \item 2 of 3 consecutive points >= 2 SD from centre
#'   \item 8 consecutive points >= 1 SD from centre (on one side)
#'   \item 9 consecutive points >= 1 SD from centre (either side, relaxed)
#'   \item 25 consecutive points with no change between successive values
#'   \item 10 consecutive alternating points
#'   \item Trend of 4 consecutive points
#'   \item Single-step difference of >= 3 units
#' }
#'
#' @param dat Numeric vector of observations (log2 titre scale).
#' @param centre Central line value.
#' @param threshold One standard deviation unit.
#'
#' @return A list of two elements:
#'   \describe{
#'     \item{\code{[[1]]}}{List of raw rule results (indices / matrices).}
#'     \item{\code{[[2]]}}{Numeric vector of length 8: proportion of
#'       observations flagged by each rule.}
#'   }
#' @examples
#' dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
#' hi_rules(dat, centre = 4, threshold = 1)
#' @export
hi_rules <- function(dat, centre, threshold) {
  res <- vector("list", 8)

  res[[1]] <- rule_xyz(dat, centre, threshold, 1, 1, 3 - 1e-5)
  res[[2]] <- rule_xyz(dat, centre, threshold, 2, 3, 2 - 1e-5)
  res[[3]] <- rule_xyz(dat, centre, threshold, 8, 8, 0)
  res[[4]] <- rule_noCrelax(dat, centre, threshold, 9)
  res[[5]] <- rule_nodiff(dat, 25)
  res[[6]] <- rule_alt(dat, 10)
  res[[7]] <- rule_trend(dat, 4)
  res[[8]] <- rule_diff(dat, 3)

  n_minus <- c(1, 3, 8, 9, 25, 10, 4, 3)
  calc    <- rep(NA_real_, 8)
  for (i in 1:8) {
    if (i < 4 || i == 8) {
      calc[i] <- length(res[[i]]) / length(dat)
    } else {
      calc[i] <- sum(res[[i]][2, ] - n_minus[i] + 1) / length(dat)
    }
  }

  list(res, calc)
}


#' Apply all eight HI control rules and return counts
#'
#' Identical to \code{\link{hi_rules}} except the second list element
#' contains raw counts of flagged observations rather than proportions.
#'
#' @inheritParams hi_rules
#'
#' @return A list of two elements:
#'   \describe{
#'     \item{\code{[[1]]}}{List of raw rule results (indices / matrices).}
#'     \item{\code{[[2]]}}{Integer vector of length 8: number of observations
#'       flagged by each rule.}
#'   }
#' @examples
#' dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
#' hi_rules2(dat, centre = 4, threshold = 1)
#' @export
hi_rules2 <- function(dat, centre, threshold) {
  res <- vector("list", 8)

  res[[1]] <- rule_xyz(dat, centre, threshold, 1, 1, 3 - 1e-5)
  res[[2]] <- rule_xyz(dat, centre, threshold, 2, 3, 2 - 1e-5)
  res[[3]] <- rule_xyz(dat, centre, threshold, 8, 8, 0)
  res[[4]] <- rule_noCrelax(dat, centre, threshold, 9)
  res[[5]] <- rule_nodiff(dat, 25)
  res[[6]] <- rule_alt(dat, 10)
  res[[7]] <- rule_trend(dat, 4)
  res[[8]] <- rule_diff(dat, 3)

  n_minus <- c(1, 3, 8, 9, 25, 10, 4, 3)
  calc    <- rep(NA_integer_, 8)
  for (i in 1:8) {
    if (i < 4 || i == 8) {
      calc[i] <- length(res[[i]])
    } else {
      calc[i] <- sum(res[[i]][2, ] - n_minus[i] + 1)
    }
  }

  list(res, calc)
}
