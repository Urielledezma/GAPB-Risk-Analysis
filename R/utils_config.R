# Configuration access.
#
# config/assets.yml and config/params.yml are the single sources of truth for
# the universe and for every model constant. Nothing in R/, analysis/ or
# reports/ restates a ticker, a window length or a confidence level -- it asks
# for one here.
#
# Both files are read once per session and cached, so repeated calls inside a
# report are free.

.config_cache <- new.env(parent = emptyenv())

#' Read a YAML configuration file from config/.
#'
#' @param name File name, with or without the .yml extension.
#' @param refresh Re-read from disk instead of using the cached value.
#' @return A named list.
load_config <- function(name, refresh = FALSE) {
  if (!grepl("\\.ya?ml$", name)) {
    name <- paste0(name, ".yml")
  }
  if (!refresh && !is.null(.config_cache[[name]])) {
    return(.config_cache[[name]])
  }
  path <- proj_path("config", name)
  if (!file.exists(path)) {
    stop("Missing configuration file: config/", name, call. = FALSE)
  }
  value <- yaml::read_yaml(path)
  .config_cache[[name]] <- value
  value
}

#' The asset configuration.
#'
#' @return A named list, as parsed from config/assets.yml.
assets_config <- function() {
  load_config("assets.yml")
}

#' The model parameter configuration.
#'
#' @return A named list, as parsed from config/params.yml.
params <- function() {
  load_config("params.yml")
}

#' The SEC filings manifest.
#'
#' Maps every reporting period to the filing that supplies it, so the sourcing
#' of a figure is configuration rather than something buried in a script.
#'
#' @return A named list, as parsed from config/filings.yml.
filings_config <- function() {
  load_config("filings.yml")
}

#' The full asset universe as a data frame.
#'
#' @return One row per asset, with ticker, name, series, sector, industry and
#'   FactSet identifier.
universe <- function() {
  rows <- assets_config()$universe
  do.call(
    rbind,
    lapply(rows, function(row) {
      data.frame(
        ticker = row$ticker,
        name = row$name,
        series = row$series,
        sector = row$sector,
        industry = row$industry,
        factset_id = row$factset_id,
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Every ticker in the universe.
#'
#' @return A character vector.
tickers <- function() {
  universe()$ticker
}

#' The primary asset under analysis.
#'
#' @return A single ticker.
subject <- function() {
  assets_config()$subject
}

#' Metadata for one asset.
#'
#' @param ticker Ticker to look up. Defaults to the subject asset.
#' @return A one-row data frame.
asset_meta <- function(ticker = subject()) {
  rows <- universe()
  match <- rows[rows$ticker == ticker, , drop = FALSE]
  if (nrow(match) == 0) {
    stop(
      "Unknown ticker: ", ticker, ".\n",
      "  Known tickers: ", paste(rows$ticker, collapse = ", "), ".\n",
      "  Add it to config/assets.yml if it belongs in the universe.",
      call. = FALSE
    )
  }
  match
}

#' The EWMA smoothing grid implied by config/params.yml.
#'
#' @return A numeric vector of candidate lambda values.
ewma_lambda_grid <- function() {
  spec <- params()$variance$ewma
  seq(spec$lambda_grid_min, spec$lambda_grid_max, by = spec$lambda_grid_by)
}

#' Apply the configured RNG seed.
#'
#' Called before any simulation so that Monte Carlo VaR and GBM paths reproduce
#' exactly across machines and re-renders.
#'
#' @return Invisibly, the seed used.
use_seed <- function() {
  seed <- params()$seed
  set.seed(seed)
  invisible(seed)
}
