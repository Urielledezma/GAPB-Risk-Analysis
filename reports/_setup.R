# Shared setup for every report.
#
# Sourced from the first chunk of each .qmd. Loads the analytics library, the
# plotting theme and the chunk defaults, so no report restates any of them.
#
# Quarto executes each document from this directory, which is why the library
# path is relative. proj_path() takes over from there and resolves everything
# else from the project root.

source(file.path("..", "R", "utils_io.R"))
source_lib()

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

ggplot2::theme_set(theme_risk())

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 4.5,
  dpi = 200,
  fig.align = "center",
  dev = "png"
)

options(
  scipen = 999,
  digits = 4,
  knitr.kable.NA = "—"
)

#' Render a data frame as a report table.
#'
#' Every multi-series figure in this project is paired with its table: the chart
#' palette carries a sub-3:1 contrast warning on three of its six slots, and a
#' readable table is one of the two accepted forms of relief.
#'
#' @param data A data frame.
#' @param caption Table caption.
#' @param digits Decimal places.
#' @return A knitr kable.
report_table <- function(data, caption = NULL, digits = 4) {
  knitr::kable(
    data,
    caption = caption,
    digits = digits,
    format.args = list(big.mark = ","),
    align = "l"
  )
}
