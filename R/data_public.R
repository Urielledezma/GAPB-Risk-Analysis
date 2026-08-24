# Public market data.
#
# Daily OHLCV for the universe, from Yahoo Finance. No credentials required,
# which is the point: everything committed under data/processed/ is derived from
# this source, so the reports reproduce for anyone who clones the repository.
#
# The adjusted close is the series every return calculation uses. The unadjusted
# close is carried alongside it so that price-level narratives can quote the
# figure a market participant actually saw on the day.

#' Fetch daily OHLCV for one ticker.
#'
#' @param ticker Yahoo symbol, e.g. "GAPB.MX".
#' @param from Start date. Defaults to the configured history start.
#' @param to End date. Defaults to today.
#' @return A tidy data frame, one row per trading day.
fetch_prices_yahoo <- function(ticker,
                               from = assets_config()$history_start,
                               to = Sys.Date()) {
  series <- quantmod::getSymbols(
    ticker,
    src = "yahoo",
    from = as.Date(from),
    to = as.Date(to),
    auto.assign = FALSE,
    warnings = FALSE
  )

  if (is.null(series) || nrow(series) == 0) {
    stop("No observations returned for ", ticker, ".", call. = FALSE)
  }

  data.frame(
    date = as.Date(zoo::index(series)),
    ticker = ticker,
    open = as.numeric(quantmod::Op(series)),
    high = as.numeric(quantmod::Hi(series)),
    low = as.numeric(quantmod::Lo(series)),
    close = as.numeric(quantmod::Cl(series)),
    adjusted = as.numeric(quantmod::Ad(series)),
    volume = as.numeric(quantmod::Vo(series)),
    stringsAsFactors = FALSE
  )
}

#' Fetch daily OHLCV for the whole universe.
#'
#' One failing ticker does not abort the run: it is reported and skipped, and
#' the caller decides whether a partial universe is acceptable. A silent partial
#' download would be worse than a loud one.
#'
#' @param symbols Character vector of Yahoo symbols.
#' @param from Start date.
#' @param to End date.
#' @param cache_raw Write each untouched download to data/raw/yahoo/.
#' @return A tidy data frame stacked across tickers, sorted by ticker and date.
fetch_universe_prices <- function(symbols = tickers(),
                                  from = assets_config()$history_start,
                                  to = Sys.Date(),
                                  cache_raw = TRUE) {
  collected <- list()
  failed <- character(0)

  for (ticker in symbols) {
    message("Fetching ", ticker, " ...")
    result <- tryCatch(
      fetch_prices_yahoo(ticker, from = from, to = to),
      error = function(e) {
        warning("  failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(result)) {
      failed <- c(failed, ticker)
      next
    }
    if (cache_raw) {
      write_raw(result, "yahoo", paste0(ticker, ".csv"))
    }
    message("  ", nrow(result), " rows, ", min(result$date), " to ", max(result$date))
    collected[[ticker]] <- result
  }

  if (length(collected) == 0) {
    stop("No ticker could be downloaded. Check the network connection.", call. = FALSE)
  }
  if (length(failed) > 0) {
    warning(
      "Universe is incomplete. Failed tickers: ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  }

  prices <- do.call(rbind, collected)
  rownames(prices) <- NULL
  prices[order(prices$ticker, prices$date), ]
}

#' Drop observations with no usable adjusted price.
#'
#' Yahoo returns the occasional row with a missing or zero adjusted close --
#' typically a holiday or a stale session that made it into the series. Those
#' rows cannot produce a log return and are removed here rather than being
#' silently absorbed by an na.rm later, where nobody would ever see them.
#'
#' @param prices A tidy price data frame.
#' @return The panel with unusable rows removed.
drop_unusable_prices <- function(prices) {
  usable <- is.finite(prices$adjusted) & prices$adjusted > 0
  dropped <- prices[!usable, ]

  if (nrow(dropped) > 0) {
    message(
      "Dropping ", nrow(dropped), " observation(s) with no usable adjusted price:"
    )
    print(
      dropped[order(dropped$ticker, dropped$date), c("ticker", "date", "close", "adjusted")],
      row.names = FALSE
    )
  }

  prices[usable, ]
}

#' Validate a price panel before it is written.
#'
#' Checks the properties the downstream analysis silently assumes: one row per
#' ticker per date, no missing adjusted prices, no non-positive prices, and
#' enough history to estimate anything.
#'
#' @param prices A tidy price data frame.
#' @param min_years Minimum years of history required per ticker.
#' @return Invisibly, the input, so the call can be chained.
validate_prices <- function(prices, min_years = 5) {
  required <- c("date", "ticker", "close", "adjusted", "volume")
  missing <- setdiff(required, names(prices))
  if (length(missing) > 0) {
    stop("Price panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  key <- paste(prices$ticker, prices$date)
  if (anyDuplicated(key) > 0) {
    stop(
      "Price panel is not one row per ticker per date: ",
      sum(duplicated(key)), " duplicate keys.",
      call. = FALSE
    )
  }

  if (any(is.na(prices$adjusted))) {
    stop(
      "Adjusted price is missing for ", sum(is.na(prices$adjusted)), " observations.",
      call. = FALSE
    )
  }

  if (any(prices$adjusted <= 0, na.rm = TRUE)) {
    stop("Adjusted price is non-positive somewhere; log returns would be undefined.", call. = FALSE)
  }

  span <- stats::aggregate(
    date ~ ticker,
    data = prices,
    FUN = function(d) as.numeric(difftime(max(d), min(d), units = "days")) / 365.25
  )
  short <- span[span$date < min_years, "ticker"]
  if (length(short) > 0) {
    warning(
      "Under ", min_years, " years of history for: ", paste(short, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(prices)
}
