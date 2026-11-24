# Stage 05. Estimate portfolio risk from the public price snapshot.
# The date on a return or scenario is its ending session. P&L is signed,
# while VaR and ES are expressed in MXN of loss.

#' Calendar-month lookback, clamped to the last day of the target month.
stage05_months_before <- function(date, months) {
  # Taking steps from month-start avoids rolling 31 August into March.
  starts <- seq(as.Date(format(date, "%Y-%m-01")), by = "-1 month",
                length.out = months + 1L)
  day <- as.integer(format(date, "%d"))
  min(starts[months + 1L] + day - 1L, starts[months] - 1L)
}

#' Wide adjusted prices on exchange sessions, retaining missing ticker quotes.
stage05_panel <- function(prices, assets) {
  prices <- prices[prices$ticker %in% assets, c("date", "ticker", "adjusted")]
  prices$date <- as.Date(prices$date)
  if (anyNA(prices$date) || anyDuplicated(prices[c("date", "ticker")])) {
    stop("Stage 05 needs one price per ticker and dated session.", call. = FALSE)
  }
  if (!all(assets %in% prices$ticker)) {
    stop("Stage 05 price data do not cover the configured universe.", call. = FALSE)
  }
  dates <- sort(unique(prices$date))
  panel <- matrix(NA_real_, nrow = length(dates), ncol = length(assets),
                  dimnames = list(as.character(dates), assets))
  valid <- is.finite(prices$adjusted) & prices$adjusted > 0
  cells <- cbind(match(prices$date[valid], dates), match(prices$ticker[valid], assets))
  panel[cells] <- prices$adjusted[valid]
  panel[rowSums(is.finite(panel)) > 0L, , drop = FALSE]
}

#' Fixed-holdings return scenarios with complete prices at both endpoints.
stage05_asset_scenarios <- function(panel, horizon) {
  # Lag on the observed exchange-session union BEFORE testing completeness.
  # Missing interior prices do not change a buy-and-hold endpoint P&L.
  if (horizon >= nrow(panel)) stop("Horizon exceeds the price history.", call. = FALSE)
  ends <- seq.int(horizon + 1L, nrow(panel))
  starts <- ends - horizon
  tradable <- stats::complete.cases(panel)
  keep <- tradable[starts] & tradable[ends]
  ends <- ends[keep]
  starts <- starts[keep]
  list(start_date = as.Date(rownames(panel)[starts]), date = as.Date(rownames(panel)[ends]),
       returns = panel[ends, , drop = FALSE] / panel[starts, , drop = FALSE] - 1)
}

#' Empirical and normal loss thresholds in pesos, by horizon and confidence.
stage05_risk <- function(scenarios, confidence, sigma, value) {
  rows <- lapply(split(scenarios, scenarios$horizon), function(s) {
    if (!nrow(s)) stop("Empty historical scenario sample.", call. = FALSE)
    cuts <- stats::quantile(s$pnl_mxn, 1 - confidence, type = 7, names = FALSE)
    # The class Rmd averages the outcomes strictly below its empirical quantile.
    # At a tied minimum, the observed minimum is the whole available loss tail.
    tail_n <- vapply(cuts, function(cut) sum(s$pnl_mxn < cut), integer(1))
    es <- vapply(cuts, function(cut) {
      tail_pnl <- s$pnl_mxn[s$pnl_mxn < cut]
      if (length(tail_pnl)) -mean(tail_pnl) else -cut
    }, numeric(1))
    data.frame(confidence = confidence, horizon = s$horizon[1], n = nrow(s),
               historical_var_mxn = -cuts,
               normal_var_mxn = value * stats::qnorm(confidence) * sigma * sqrt(s$horizon[1]),
               historical_es_mxn = es, n_below_quantile = tail_n)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$horizon, out$confidence), ]
}

#' Re-estimate one-day limits using only returns available before each session.
stage05_backtest <- function(daily, asset_returns, weights, value, spec, first_day) {
  d <- daily$date
  conf <- spec$confidence
  months <- spec$parametric_window_months
  targets <- which(d >= first_day & format(d, "%Y") == format(first_day, "%Y"))
  rows <- lapply(targets, function(i) {
    lower <- stage05_months_before(d[i], months)
    prior <- seq_len(i - 1L)
    rolling <- prior[d[prior] >= lower]
    if (min(daily$start_date) > lower || length(rolling) < 2L) return(NULL)
    historical <- -stats::quantile(daily$pnl_mxn[prior], 1 - conf, names = FALSE, type = 7)
    sigma <- portfolio_sigma(weights, stats::cov(asset_returns[rolling, , drop = FALSE]))
    threshold <- c(historical, value * stats::qnorm(conf) * sigma)
    data.frame(date = d[i], method = rep(c("historical", "normal"), each = length(conf)),
               confidence = rep(conf, 2), portfolio_return = daily$portfolio_return[i],
               pnl_mxn = daily$pnl_mxn[i], var_mxn = threshold,
               breach = -daily$pnl_mxn[i] > threshold,
               train_first = rep(d[c(prior[1], rolling[1])], each = length(conf)),
               train_last = d[i - 1L],
               train_n = rep(c(length(prior), length(rolling)), each = length(conf)))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) stop("No eligible sessions in the backtest year.", call. = FALSE)
  out
}

#' Summarise expected and realised breaches, including zero-breach samples.
stage05_coverage <- function(backtest, alpha) {
  groups <- split(backtest, list(backtest$method, backtest$confidence), drop = TRUE)
  out <- do.call(rbind, lapply(groups, function(b) {
    conf <- b$confidence[1]
    coverage <- backtest_var(b$pnl_mxn, -b$var_mxn, conf, alpha)
    # At zero or all exceptions, the limiting log likelihood remains finite.
    if (is.na(coverage$lr_statistic)) {
      x <- coverage$observed_breaches
      n <- coverage$n
      coverage$lr_statistic <- 2 * (stats::dbinom(x, n, x / n, log = TRUE) -
                                      stats::dbinom(x, n, 1 - conf, log = TRUE))
      coverage$p_value <- stats::pchisq(coverage$lr_statistic, df = 1, lower.tail = FALSE)
      coverage$reject_model <- coverage$p_value < alpha
    }
    data.frame(method = b$method[1], coverage)
  }))
  rownames(out) <- NULL
  out
}

#' Locate the subject's largest absolute return in each October studied in stage 04.
stage05_cases <- function(daily, asset_returns) {
  dates <- daily$date
  asset <- subject()
  gap_return <- asset_returns[, asset]
  selected <- integer()
  for (month in c("2023-10", "2026-10")) {
    at <- which(format(dates, "%Y-%m") == month)
    if (!length(at)) next
    selected[paste0(month, "_subject_abs_max")] <- at[which.max(abs(gap_return[at]))]
  }
  data.frame(case = names(selected), date = dates[selected], row.names = NULL)
}

#' Estimate stage 05 from a long panel of raw and adjusted equity closes.
#' @param prices Data frame with date, ticker, close and adjusted.
#' @return Holdings, scenarios, six-month covariance and out-of-sample results.
stage05_portfolio <- function(prices) {
  spec <- params()$var
  panel <- stage05_panel(prices, sort(tickers()))
  complete_px <- panel[stats::complete.cases(panel), , drop = FALSE]
  common_dates <- as.Date(rownames(complete_px))
  if (!length(common_dates)) stop("No common six-asset valuation date.", call. = FALSE)
  portfolio <- build_portfolio(prices[prices$ticker %in% colnames(panel), ],
                               as_of = max(common_dates), position = spec$position_mxn)
  closes <- prices[prices$date == portfolio$as_of, c("ticker", "close")]
  portfolio$holdings$price <- closes$close[match(portfolio$holdings$ticker, closes$ticker)]
  stopifnot(all(is.finite(portfolio$holdings$price) & portfolio$holdings$price > 0))
  portfolio$holdings$shares <- portfolio$holdings$value / portfolio$holdings$price
  weights <- portfolio$weights[colnames(panel)]
  one <- stage05_asset_scenarios(panel, 1L)
  scenarios <- lapply(spec$horizons_days, function(h) {
    a <- if (h == 1L) one else stage05_asset_scenarios(panel, h)
    if (!nrow(a$returns)) stop("No complete endpoints at horizon ", h, ".", call. = FALSE)
    r <- as.numeric(a$returns %*% weights)
    data.frame(start_date = a$start_date, date = a$date, horizon = h,
               portfolio_return = r, pnl_mxn = portfolio$value * r)
  })
  scenarios <- do.call(rbind, scenarios)
  daily <- scenarios[scenarios$horizon == 1L, ]
  # As of the close: (cutoff minus six calendar months, cutoff]. For the
  # forecast of day t use only return end dates in [t minus six months, t).
  lower <- stage05_months_before(portfolio$as_of, spec$parametric_window_months)
  in_window <- one$date > lower & one$date <= portfolio$as_of
  if (min(common_dates) > lower || sum(in_window) < 2L) {
    stop("Insufficient history for the covariance window.", call. = FALSE)
  }
  cov <- stats::cov(one$returns[in_window, , drop = FALSE])
  sigma <- portfolio_sigma(weights, cov)
  risk <- stage05_risk(scenarios, spec$confidence, sigma, portfolio$value)
  backtest <- stage05_backtest(daily, one$returns, weights, portfolio$value, spec,
                               as.Date(params()$sample$oos_start))
  c(portfolio, list(prices = complete_px, common_dates = common_dates, asset_returns = one$returns,
                    session_dates = as.Date(rownames(panel)), daily = daily, scenarios = scenarios,
                    cov_dates = one$date[in_window], covariance = cov, sigma_daily = sigma,
                    risk = risk, backtest = backtest,
                    coverage = stage05_coverage(backtest, spec$backtest$alpha),
                    cases = stage05_cases(daily, one$returns)))
}
