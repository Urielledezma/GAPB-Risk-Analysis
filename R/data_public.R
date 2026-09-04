# Public market data -- the fallback source.
#
# Daily OHLCV from Yahoo Finance. FactSet is the primary path (see
# R/data_factset.R and R/data_sources.R); this loader runs when a key is absent,
# unentitled, or the vendor is unreachable.
#
# It is not merely a spare tyre. Because it needs no credentials, it is also the
# only source whose output can be committed to a public repository, which makes
# it the reproducibility floor: anyone can clone the project and render every
# report, and the FactSet numbers override it wherever a key is present.
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
    source = "yahoo",
    stringsAsFactors = FALSE
  )
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

#' Round a price panel to a sane precision before committing it.
#'
#' Yahoo returns adjusted prices to fifteen significant digits, which triples
#' the size of a file that git stores in full on every refresh. Four decimals is
#' several orders of magnitude finer than the centavo tick, and the resulting
#' error in a log return is around 1e-7 against a daily volatility near 2e-2 --
#' invisible in any figure this project reports.
#'
#' @param prices A tidy price data frame.
#' @param digits Decimal places for price columns.
#' @return The panel with price columns rounded and volume stored as an integer.
round_prices <- function(prices, digits = 4) {
  for (column in intersect(c("open", "high", "low", "close", "adjusted"), names(prices))) {
    prices[[column]] <- round(prices[[column]], digits)
  }
  if ("volume" %in% names(prices)) {
    prices$volume <- as.integer(round(prices$volume))
  }
  prices
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
