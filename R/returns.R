# Returns, annualisation and drawdowns.
#
# The project works exclusively in continuously compounded (log) returns:
#
#   r_t = ln(S_t / S_{t-1})
#
# They are additive across time, which is what makes the horizon scaling in
# stages 03 and 05 -- and the annualisation below -- exact rather than
# approximate.

#' Daily log returns from a tidy price panel.
#'
#' @param prices A price panel with date, ticker and a price column.
#' @param price_col Price column to use. Defaults to the configured field,
#'   which is the adjusted close.
#' @return A data frame with date, ticker and log_return. The first observation
#'   of each ticker is dropped, having no predecessor.
compute_log_returns <- function(prices, price_col = params()$conventions$price_field) {
  stopifnot(all(c("date", "ticker", price_col) %in% names(prices)))

  prices <- prices[order(prices$ticker, prices$date), ]
  split_by_ticker <- split(prices, prices$ticker)

  rows <- lapply(split_by_ticker, function(one) {
    price <- one[[price_col]]
    if (length(price) < 2) {
      return(NULL)
    }
    out <- data.frame(
      date = one$date[-1],
      ticker = one$ticker[-1],
      log_return = diff(log(price)),
      stringsAsFactors = FALSE
    )
    # Provenance travels with the derived series: a return computed from vendor
    # prices is vendor-derived and carries the same redistribution restriction.
    if ("source" %in% names(one)) {
      out$source <- one$source[-1]
    }
    out
  })

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(out) <- NULL
  out[order(out$ticker, out$date), ]
}

#' Restrict a dated data frame to a sample window.
#'
#' @param data A data frame with a date column.
#' @param start Inclusive start. Defaults to the configured sample start.
#' @param end Inclusive end. Defaults to the configured cut-off.
#' @return The filtered data frame.
in_sample <- function(data,
                      start = params()$sample$start,
                      end = params()$sample$end) {
  data[data$date >= as.Date(start) & data$date <= as.Date(end), , drop = FALSE]
}

#' Annualise a daily mean return.
#'
#' Log returns are additive, so the annual mean is the daily mean times the
#' number of trading days.
#'
#' @param mu_daily Daily mean log return.
#' @param trading_days Trading days per year.
#' @return The annualised mean.
annualise_mean <- function(mu_daily, trading_days = params()$conventions$trading_days_per_year) {
  mu_daily * trading_days
}

#' Annualise a daily volatility.
#'
#' Under the i.i.d. assumption variance scales linearly with time, so the
#' standard deviation scales with its square root.
#'
#' @param sigma_daily Daily standard deviation of log returns.
#' @param trading_days Trading days per year.
#' @return The annualised volatility.
annualise_vol <- function(sigma_daily, trading_days = params()$conventions$trading_days_per_year) {
  sigma_daily * sqrt(trading_days)
}

#' Daily and annualised moments for a return series.
#'
#' @param returns Numeric vector of log returns.
#' @param trading_days Trading days per year.
#' @return A one-row data frame: n, mean and volatility, daily and annualised.
return_moments <- function(returns,
                           trading_days = params()$conventions$trading_days_per_year) {
  returns <- returns[is.finite(returns)]
  mu <- mean(returns)
  sigma <- stats::sd(returns)
  data.frame(
    n = length(returns),
    mean_daily = mu,
    vol_daily = sigma,
    mean_annual = annualise_mean(mu, trading_days),
    vol_annual = annualise_vol(sigma, trading_days),
    stringsAsFactors = FALSE
  )
}

#' Moments year by year.
#'
#' Stage 03 reports the whole sample and then each year separately, because a
#' single pooled volatility hides exactly the structural breaks the analysis is
#' looking for.
#'
#' Two figures are easy to conflate here. mean_annual annualises the year's
#' average day, 252 times its mean, whatever the number of sessions it actually
#' had. realised_annual is what the year actually returned: the sum of its log
#' returns, which telescopes to ln(P_last / P_last_of_previous_year). They agree
#' only for a complete year of exactly 252 sessions.
#'
#' @param returns A data frame with date and log_return.
#' @param years Years to report. Defaults to the configured breakdown.
#' @return One row per year.
return_moments_by_year <- function(returns, years = params()$sample$annual_breakdown) {
  rows <- lapply(years, function(year) {
    slice <- returns[format(returns$date, "%Y") == as.character(year), ]
    if (nrow(slice) == 0) {
      return(NULL)
    }
    cbind(
      year = year,
      return_moments(slice$log_return),
      realised_annual = sum(slice$log_return[is.finite(slice$log_return)])
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(out) <- NULL
  out
}

#' The last available price on or before each target date.
#'
#' Calendar horizons -- three months after a date -- often land on a weekend or a
#' holiday. The price that a holder could actually observe is the last close on
#' or before that date, never a later one.
#'
#' @param prices A data frame with date and a price column.
#' @param dates Target dates.
#' @param price_col Price column to read.
#' @return One row per target: the target, the date actually used and its
#'   price. Targets before the first observation, or after the last one, return
#'   NA, because a price beyond the data cannot be inferred from it.
price_on_or_before <- function(prices, dates, price_col = params()$conventions$price_field) {
  prices$date <- as.Date(prices$date)
  prices <- prices[order(prices$date), ]
  dates <- as.Date(dates)
  index <- findInterval(as.numeric(dates), as.numeric(prices$date))
  beyond <- dates > max(prices$date)
  index[index == 0 | beyond] <- NA

  data.frame(
    target = dates,
    date = prices$date[index],
    price = prices[[price_col]][index],
    stringsAsFactors = FALSE
  )
}

#' The price a given number of sessions after a date.
#'
#' @param prices A data frame with date and a price column.
#' @param from The starting session, which must be in the data.
#' @param sessions Numbers of sessions ahead, zero or more.
#' @param price_col Price column to read.
#' @return One row per offset: sessions, date and price; NA past the data.
price_after_sessions <- function(prices, from, sessions,
                                 price_col = params()$conventions$price_field) {
  if (any(sessions < 0)) {
    stop("price_after_sessions: sessions must be zero or positive.", call. = FALSE)
  }
  prices <- prices[order(prices$date), ]
  start <- match(as.Date(from), prices$date)
  if (is.na(start)) {
    stop("price_after_sessions: ", format(as.Date(from)), " is not a session in the data.",
         call. = FALSE)
  }
  index <- start + sessions
  index[index > nrow(prices)] <- NA

  data.frame(
    sessions = sessions,
    date = prices$date[index],
    price = prices[[price_col]][index],
    stringsAsFactors = FALSE
  )
}

#' Cumulative wealth index from log returns.
#'
#' @param returns Numeric vector of log returns.
#' @return A numeric vector starting at 1.
wealth_index <- function(returns) {
  exp(cumsum(c(0, returns[is.finite(returns)])))
}

#' Drawdown series from a price or wealth path.
#'
#' Drawdown at t is the proportional decline from the running maximum:
#'
#'   DD_t = S_t / max(S_1..S_t) - 1
#'
#' @param path Numeric vector of prices or wealth levels.
#' @return A numeric vector of drawdowns, zero or negative.
drawdown_series <- function(path) {
  path <- as.numeric(path)
  path / cummax(path) - 1
}

#' Maximum drawdown and the dates that bracket it.
#'
#' @param path Numeric vector of prices or wealth levels.
#' @param dates Optional dates aligned with path.
#' @return A one-row data frame: max_drawdown, and peak, trough and recovery
#'   dates when dates are supplied.
max_drawdown <- function(path, dates = NULL) {
  drawdown <- drawdown_series(path)
  trough_index <- which.min(drawdown)
  peak_index <- which.max(path[seq_len(trough_index)])

  out <- data.frame(
    max_drawdown = drawdown[trough_index],
    stringsAsFactors = FALSE
  )

  if (!is.null(dates)) {
    recovered <- which(
      seq_along(path) > trough_index & path >= path[peak_index]
    )
    out$peak_date <- dates[peak_index]
    out$trough_date <- dates[trough_index]
    out$recovery_date <- if (length(recovered) > 0) dates[recovered[1]] else as.Date(NA)
  }

  out
}

#' Pivot a tidy return panel into a date-by-ticker matrix.
#'
#' Only dates on which every ticker traded are kept, since a covariance matrix
#' estimated on ragged data is not a covariance matrix.
#'
#' @param returns A data frame with date, ticker and log_return.
#' @return A numeric matrix with dates as row names.
returns_matrix <- function(returns) {
  wide <- stats::reshape(
    returns[, c("date", "ticker", "log_return")],
    idvar = "date",
    timevar = "ticker",
    direction = "wide"
  )
  names(wide) <- sub("^log_return\\.", "", names(wide))
  wide <- wide[order(wide$date), ]
  complete <- stats::complete.cases(wide)

  matrix_out <- as.matrix(wide[complete, setdiff(names(wide), "date"), drop = FALSE])
  rownames(matrix_out) <- format(wide$date[complete])
  matrix_out
}
