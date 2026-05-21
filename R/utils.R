#' Convert HI titre strings to log2 scale
#'
#' Handles numeric titres and left-censored values prefixed with \code{"<"}.
#' The conversion is \eqn{\log_2(\text{titre} / 10)}, so a titre of 40
#' becomes 2 on the log2 scale. Left-censored values (e.g. \code{"<10"})
#' are treated as half the detection limit.
#'
#' @param titres Character vector of HI titres (e.g. \code{c("40", "<10",
#'   "160")}).
#'
#' @return Numeric vector of log2-transformed values.
#' @examples
#' log_num(c("40", "80", "<10", "160"))
#' @export
log_num <- function(titres) {
  num <- log(as.numeric(titres) / 10, 2)
  censored <- grep("<", titres)
  num[censored] <- log(as.numeric(gsub("<", "", titres)) / 20, 2)[censored]
  return(num)
}


#' Plot a reference panel control chart from an Acmacs map
#'
#' For each antigen/serum combination in \code{map} with at least
#' \code{min_n} repeat titres, applies \code{\link{hi_rules2}} and draws a
#' compact control chart via \code{\link{plot_hi2}}. Output is written to a
#' PDF file.
#'
#' Requires the \pkg{Racmacs} package.
#'
#' @param map An Acmacs map object (from \pkg{Racmacs}).
#' @param name Character string appended to the default output filename.
#' @param min_n Minimum number of non-missing repeat titres required for a
#'   cell to be included. Default \code{5}.
#' @param file Output file path. Defaults to
#'   \code{paste0("reference-panel-plot", name, ".pdf")} in the current
#'   working directory.
#'
#' @return Called for its side effect (writes a PDF). Returns \code{NULL}
#'   invisibly.
#' @examples
#' \dontrun{
#' map <- Racmacs::read.acmap("my_map.ace")
#' ref_panel_plot(map, name = "2024H", file = tempfile(fileext = ".pdf"))
#' }
#' @export
ref_panel_plot <- function(map, name, min_n = 5,
                           file = paste0("reference-panel-plot", name, ".pdf")) {
  if (!requireNamespace("Racmacs", quietly = TRUE)) {
    stop("Package 'Racmacs' is required for ref_panel_plot().")
  }

  ttl     <- Racmacs::titerTableLayers(map)
  ti_char <- ti_num <- NULL
  rep_mat <- matrix(ncol = Racmacs::numSera(map), nrow = Racmacs::numAntigens(map))

  for (i in Racmacs::numAntigens(map):1) {
    for (j in Racmacs::numSera(map):1) {
      ti_char[[i]][[j]] <- sapply(ttl, "[[", i, j)
      ti_num[[i]][[j]]  <- log_num(ti_char[[i]][[j]])
      rep_mat[i, j]     <- length(ti_char[[i]][[j]]) -
                           sum(ti_char[[i]][[j]] == "*") -
                           sum(ti_char[[i]][[j]] == ".")
    }
  }

  n_row <- sum(rowSums(rep_mat > min_n) > 1)
  n_col <- sum(colSums(rep_mat > min_n) > 1)

  cols       <- c("red", "orange", "yellow", "tan", "seagreen", "purple", "blue", "magenta")
  rule_names <- c("1 >=3", "2/3 >=2", "8 >=1", "9 >=1 both",
                  "no diff 25", "alt 10", "trend 4", "diff of 3")

  old_par <- par("mfrow", "mar")
  on.exit(par(old_par), add = TRUE)
  pdf(file, width = n_col, height = n_row / 2)
  par(mfrow = c(n_row, n_col), mar = rep(0, 4))

  for (i in which(rowSums(rep_mat > min_n) > 1)) {
    for (j in which(colSums(rep_mat > min_n) > 1)) {
      if (sum(!is.na(ti_num[[i]][[j]])) > 0) {
        med <- median(ti_num[[i]][[j]], na.rm = TRUE)
        out <- hi_rules2(ti_num[[i]][[j]], med, 1)
        plot_hi2(ti_num[[i]][[j]], out[[1]], med, 1,
                 c(1, 3, 8, 9, 25, 10, 4, 2), length(ttl))
        text(length(ttl) - 1, -1:10, as.character(10 * 2^c(-1:10)),
             pos = 4, offset = 0, cex = 0.175, col = "grey95")
        legend("topright",
               paste0("SD=", round(sd(ti_num[[i]][[j]], na.rm = TRUE), 2)),
               bty = "n", cex = 0.175)
        legend("bottomright",
               paste0(rule_names, ": ", out[[2]]),
               bty = "n", cex = 0.175, text.col = cols)
      } else {
        plot(NA, axes = FALSE, frame.plot = TRUE,
             ylim = c(-1.5, 10), xlim = c(0.7, length(ttl) + 0.3))
      }
    }
  }

  dev.off()
  invisible(NULL)
}
