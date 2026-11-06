# Archived from R/plots.R and R/returns.R on 2026-11-05. Not sourced, linted or tested.
#
# No report, script or function called either helper. Reports render their
# figures inline, and stage 02 computes its wealth paths in the report.

#' Save a figure to outputs/figures/.
#'
#' outputs/ is untracked: figures are regenerated from the committed data rather
#' than versioned alongside it.
#'
#' @param plot A ggplot object.
#' @param name File name without extension.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution.
#' @return Invisibly, the path written.
save_figure <- function(plot, name, width = 8, height = 4.5, dpi = 200) {
  path <- proj_path("outputs", "figures", paste0(name, ".png"))
  ensure_dir(dirname(path))
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi, bg = RISK_INK$surface)
  invisible(path)
}

#' Cumulative wealth index from log returns.
#'
#' @param returns Numeric vector of log returns.
#' @return A numeric vector starting at 1.
wealth_index <- function(returns) {
  exp(cumsum(c(0, returns[is.finite(returns)])))
}
