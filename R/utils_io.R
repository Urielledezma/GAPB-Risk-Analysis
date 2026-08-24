# Project paths and dataset I/O.
#
# Every path in the project is resolved from the project root rather than from
# the working directory, so a script behaves the same whether it is sourced from
# RStudio, run with Rscript from the repository root, or knitted by Quarto from
# inside reports/.

#' Locate the project root by walking up for the .Rproj file.
#'
#' @param start Directory to start from.
#' @return Absolute path to the project root.
find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (length(list.files(path, pattern = "\\.Rproj$")) > 0) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop(
        "Could not locate the project root: no .Rproj file found above '",
        start, "'.",
        call. = FALSE
      )
    }
    path <- parent
  }
}

#' Build an absolute path from the project root.
#'
#' @param ... Path components, as in file.path().
#' @return An absolute path.
proj_path <- function(...) {
  file.path(find_project_root(), ...)
}

#' Create a directory if it does not already exist.
#'
#' @param path Directory path.
#' @return Invisibly, the path.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

#' Load the analytics library.
#'
#' Sources every file in R/ except this one and any file already sourced. Called
#' at the top of each ingest script and each report.
#'
#' @param exclude File names to skip.
#' @return Invisibly, the files sourced.
source_lib <- function(exclude = character(0)) {
  files <- list.files(proj_path("R"), pattern = "\\.R$", full.names = TRUE)
  files <- files[!basename(files) %in% exclude]
  for (file in files) {
    source(file, local = FALSE, encoding = "UTF-8")
  }
  invisible(files)
}

#' Write an analysis-ready dataset to data/processed/.
#'
#' Written as UTF-8 CSV with ISO dates so the file diffs cleanly in git and
#' opens the same way on every platform.
#'
#' @param data A data frame.
#' @param name File name, with or without the .csv extension.
#' @return Invisibly, the path written.
write_processed <- function(data, name) {
  stopifnot(is.data.frame(data))
  if (!grepl("\\.csv$", name)) {
    name <- paste0(name, ".csv")
  }
  path <- proj_path("data", "processed", name)
  ensure_dir(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  message("Wrote ", nrow(data), " rows to data/processed/", name)
  invisible(path)
}

#' Read a dataset from data/processed/.
#'
#' This is the only door reports are allowed to use. A missing file is an
#' instruction to run the ingest pipeline, not an error to work around.
#'
#' @param name File name, with or without the .csv extension.
#' @param date_cols Columns to parse as Date.
#' @return A data frame.
read_processed <- function(name, date_cols = "date") {
  if (!grepl("\\.csv$", name)) {
    name <- paste0(name, ".csv")
  }
  path <- proj_path("data", "processed", name)
  if (!file.exists(path)) {
    stop(
      "Missing dataset: data/processed/", name, ".\n",
      "  Run the ingest pipeline first: source('analysis/04_build_datasets.R').",
      call. = FALSE
    )
  }
  data <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  for (column in intersect(date_cols, names(data))) {
    data[[column]] <- as.Date(data[[column]])
  }
  data
}

#' Write an untouched vendor extract to data/raw/.
#'
#' data/raw/ is excluded from version control. Anything written here stays on
#' this machine.
#'
#' @param data A data frame.
#' @param ... Path components below data/raw/.
#' @return Invisibly, the path written.
write_raw <- function(data, ...) {
  path <- proj_path("data", "raw", ...)
  ensure_dir(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}
