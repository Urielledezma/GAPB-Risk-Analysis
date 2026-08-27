# The market data source ladder.
#
# FactSet first, the public source only if FactSet cannot answer. This is the
# one place that decides where a price comes from; nothing else in the project
# knows or cares.
#
# Three rules the implementation enforces:
#
#   1. A fallback is always announced. A run that quietly degraded to Yahoo and
#      a run that used FactSet throughout must never look identical in the log
#      or in the output.
#   2. Provenance travels with the data. Every row carries a source column, and
#      every ingest writes a manifest recording which source served which
#      ticker, when, and how many rows.
#   3. Mixed provenance is a warning, not a shrug. A panel that is FactSet for
#      five tickers and Yahoo for the sixth has a seam in it, and the seam
#      belongs in the report's methodology note.

PRICE_SCHEMA <- c(
  "date", "ticker", "open", "high", "low", "close", "adjusted", "volume", "source"
)

#' Conform a price frame to the shared schema.
#'
#' Both loaders return the same columns in the same order, so downstream code
#' never has to ask which one produced a frame.
#'
#' @param prices A price data frame from either loader.
#' @return The frame with the schema's columns, in order.
conform_price_schema <- function(prices) {
  for (column in setdiff(PRICE_SCHEMA, names(prices))) {
    prices[[column]] <- NA
  }
  prices[, PRICE_SCHEMA, drop = FALSE]
}

#' Fetch daily prices for one ticker, down the source ladder.
#'
#' @param ticker Canonical ticker, as listed in config/assets.yml.
#' @param from Start date.
#' @param to End date.
#' @param allow_fallback Permit the public source when FactSet fails.
#' @return A tidy price frame, or NULL when every source failed.
fetch_prices <- function(ticker,
                         from = assets_config()$history_start,
                         to = Sys.Date(),
                         allow_fallback = assets_config()$sources$allow_fallback) {
  meta <- asset_meta(ticker)

  primary <- tryCatch(
    {
      result <- fetch_factset_prices(
        ids = meta$factset_id,
        tickers_out = ticker,
        start = from,
        end = format(as.Date(to), "%Y-%m-%d")
      )
      message("  factset: ", nrow(result), " rows")
      conform_price_schema(result)
    },
    error = function(e) {
      message("  factset unavailable: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(primary)) {
    return(primary)
  }

  if (!isTRUE(allow_fallback)) {
    warning(
      "FactSet failed for ", ticker,
      " and the fallback is disabled (sources.allow_fallback).",
      call. = FALSE
    )
    return(NULL)
  }

  message("  falling back to the public source")
  tryCatch(
    {
      result <- fetch_prices_yahoo(ticker, from = from, to = to)
      message("  yahoo: ", nrow(result), " rows")
      conform_price_schema(result)
    },
    error = function(e) {
      warning("  fallback also failed: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

#' Fetch daily prices for a set of tickers, down the source ladder.
#'
#' @param symbols Tickers to fetch. Defaults to the subject asset alone, because
#'   stages 01 to 04 need nothing else and a full-universe pull spends vendor
#'   quota for data no report reads until stage 05.
#' @param from Start date.
#' @param to End date.
#' @param allow_fallback Permit the public source when FactSet fails.
#' @return A tidy price frame stacked across tickers.
fetch_prices_for <- function(symbols = subject(),
                             from = assets_config()$history_start,
                             to = Sys.Date(),
                             allow_fallback = assets_config()$sources$allow_fallback) {
  collected <- list()
  failed <- character(0)

  for (ticker in symbols) {
    message("Fetching ", ticker, " ...")
    result <- fetch_prices(ticker, from = from, to = to, allow_fallback = allow_fallback)
    if (is.null(result)) {
      failed <- c(failed, ticker)
      next
    }
    collected[[ticker]] <- result
  }

  if (length(collected) == 0) {
    stop("No ticker could be retrieved from any source.", call. = FALSE)
  }
  if (length(failed) > 0) {
    warning(
      "Incomplete universe. Failed tickers: ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  }

  prices <- do.call(rbind, collected)
  rownames(prices) <- NULL
  prices[order(prices$ticker, prices$date), ]
}

#' Summarise where each ticker's data came from.
#'
#' @param prices A tidy price frame carrying a source column.
#' @return One row per ticker and source.
provenance <- function(prices) {
  out <- do.call(rbind, lapply(
    split(prices, list(prices$ticker, prices$source), drop = TRUE),
    function(one) {
      data.frame(
        ticker = unique(one$ticker),
        source = unique(one$source),
        rows = nrow(one),
        first = format(min(one$date)),
        last = format(max(one$date)),
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(out) <- NULL
  out[order(out$ticker), ]
}

#' Write the provenance manifest and warn on mixed sources.
#'
#' The manifest is written beside the dataset it describes, so a reader can tell
#' which vendor produced the numbers without re-running anything. It follows the
#' data into data/private/ when the data is vendor-derived -- not because the
#' manifest is sensitive, but because a tracked file whose contents depend on
#' who ran the ingest would churn in every pull request.
#'
#' @param prices A tidy price frame.
#' @param name Manifest file name.
#' @return Invisibly, the provenance table.
record_provenance <- function(prices, name = "prices_provenance") {
  manifest <- provenance(prices)
  manifest$recorded_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  print(manifest[, c("ticker", "source", "rows", "first", "last")], row.names = FALSE)

  sources <- unique(manifest$source)
  if (length(sources) > 1) {
    warning(
      "Mixed provenance: ", paste(sources, collapse = " and "),
      ". The seam belongs in the methodology note of any report using this panel.",
      call. = FALSE
    )
  } else if (identical(sources, "yahoo")) {
    message(
      "\nEvery row came from the public fallback. FactSet was not reached; ",
      "check the credentials and entitlement before treating these as final."
    )
  }

  if (any(prices$source == "factset")) {
    write_private(manifest, name)
  } else {
    write_processed(manifest, name)
  }
  invisible(manifest)
}
