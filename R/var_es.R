# Value at Risk and Expected Shortfall.
#
# Sign convention, fixed here and used everywhere: VaR and ES are reported as
# POSITIVE loss magnitudes. A 99% one-day VaR of 12,400 means "a loss of 12,400
# or worse is expected on 1% of days". Returns keep their natural sign; only the
# risk measures are flipped. Mixing the two conventions in one report is the
# most common way these tables end up wrong.
#
# Horizon scaling assumes returns are serially independent, so variance grows
# linearly with time and the scaling factor is sqrt(h). Stage 03 tests that
# assumption rather than asserting it, and where it fails the scaled figures
# should be read as indicative.

#' Historical-simulation VaR.
#'
#' Non-parametric: the quantile is read directly off the empirical distribution,
#' so fat tails and skew are carried through exactly as the sample exhibits
#' them, with no distributional assumption.
#'
#' @param returns Numeric vector of log returns.
#' @param confidence Confidence level, e.g. 0.99.
#' @param horizon Horizon in days.
#' @param value Position or portfolio value.
#' @param method "scale" applies sqrt(h) to the one-day quantile; "overlap"
#'   reads the quantile off overlapping h-day returns, which respects any
#'   autocorrelation at the cost of a smaller effective sample.
#' @return A one-row data frame: confidence, horizon, VaR as a return, VaR in
#'   currency, and VaR as a percentage of value.
var_historical <- function(returns, confidence, horizon = 1, value = 1,
                           method = c("scale", "overlap")) {
  method <- match.arg(method)
  returns <- returns[is.finite(returns)]

  if (method == "scale" || horizon == 1) {
    quantile_return <- stats::quantile(returns, probs = 1 - confidence, names = FALSE)
    var_return <- quantile_return * sqrt(horizon)
  } else {
    aggregated <- stats::filter(returns, rep(1, horizon), sides = 1)
    aggregated <- aggregated[is.finite(aggregated)]
    var_return <- stats::quantile(aggregated, probs = 1 - confidence, names = FALSE)
  }

  var_currency <- value * (1 - exp(var_return))

  data.frame(
    method = paste0("historical (", method, ")"),
    confidence = confidence,
    horizon = horizon,
    var_return = var_return,
    var_currency = var_currency,
    var_pct = var_currency / value,
    stringsAsFactors = FALSE
  )
}

#' Parametric (variance-covariance) VaR.
#'
#' Assumes normally distributed returns, which stage 03 shows they are not. It
#' is reported anyway, and the gap between it and the historical figure is one
#' of the findings rather than an embarrassment: the size of that gap is a
#' direct measure of what the normality assumption costs.
#'
#' @param sigma Daily volatility of the position or portfolio.
#' @param confidence Confidence level.
#' @param horizon Horizon in days.
#' @param value Position or portfolio value.
#' @param mu Daily mean return. Defaults to zero, the usual convention at short
#'   horizons.
#' @return A one-row data frame, same shape as var_historical().
var_parametric <- function(sigma, confidence, horizon = 1, value = 1, mu = 0) {
  z <- stats::qnorm(1 - confidence)
  var_return <- mu * horizon + z * sigma * sqrt(horizon)
  var_currency <- value * (1 - exp(var_return))

  data.frame(
    method = "parametric",
    confidence = confidence,
    horizon = horizon,
    var_return = var_return,
    var_currency = var_currency,
    var_pct = var_currency / value,
    stringsAsFactors = FALSE
  )
}

#' Monte Carlo VaR and ES by bootstrap resampling.
#'
#' Non-parametric Monte Carlo: draws are resampled from the empirical return
#' distribution rather than from a fitted normal, so the simulated tail inherits
#' the shape of the observed one.
#'
#' @param returns Numeric vector of log returns.
#' @param confidence Confidence level.
#' @param horizon Horizon in days.
#' @param value Position or portfolio value.
#' @param n_simulations Number of simulated paths.
#' @return A one-row data frame with both VaR and ES, in return, currency and
#'   percentage terms.
var_monte_carlo <- function(returns, confidence, horizon = 1, value = 1,
                            n_simulations = params()$var$monte_carlo$n_simulations) {
  returns <- returns[is.finite(returns)]
  use_seed()

  draws <- matrix(
    sample(returns, size = n_simulations * horizon, replace = TRUE),
    nrow = horizon
  )
  simulated <- colSums(draws)

  var_return <- stats::quantile(simulated, probs = 1 - confidence, names = FALSE)
  tail_losses <- simulated[simulated <= var_return]
  es_return <- mean(tail_losses)

  var_currency <- value * (1 - exp(var_return))
  es_currency <- value * (1 - exp(es_return))

  data.frame(
    method = "monte carlo (bootstrap)",
    confidence = confidence,
    horizon = horizon,
    n_simulations = n_simulations,
    var_return = var_return,
    var_currency = var_currency,
    var_pct = var_currency / value,
    es_return = es_return,
    es_currency = es_currency,
    es_pct = es_currency / value,
    stringsAsFactors = FALSE
  )
}

#' Historical Expected Shortfall.
#'
#' The average loss conditional on breaching the VaR threshold. Where VaR says
#' how bad things get before the worst 1%, ES says how bad the worst 1% actually
#' is -- the question VaR is structurally incapable of answering.
#'
#' @param returns Numeric vector of log returns.
#' @param confidence Confidence level.
#' @param value Position or portfolio value.
#' @return A one-row data frame with ES in return, currency and percentage terms.
expected_shortfall <- function(returns, confidence, value = 1) {
  returns <- returns[is.finite(returns)]
  threshold <- stats::quantile(returns, probs = 1 - confidence, names = FALSE)
  tail_losses <- returns[returns <= threshold]
  es_return <- mean(tail_losses)
  es_currency <- value * (1 - exp(es_return))

  data.frame(
    method = "historical",
    confidence = confidence,
    n_tail = length(tail_losses),
    es_return = es_return,
    es_currency = es_currency,
    es_pct = es_currency / value,
    stringsAsFactors = FALSE
  )
}

#' Rolling one-day VaR series for backtesting.
#'
#' At each date the VaR is estimated on the preceding `window` observations
#' only, so the series is what the model would genuinely have reported on the
#' day. Estimating it on the full sample and then counting breaches against the
#' same sample is not a backtest.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param confidence Confidence level.
#' @param window Rolling estimation window in observations.
#' @param method "historical" or "parametric".
#' @return A numeric vector of VaR returns (negative), aligned with returns and
#'   NA for the first `window` entries.
rolling_var <- function(returns, confidence,
                        window = params()$var$backtest$window,
                        method = c("historical", "parametric")) {
  method <- match.arg(method)
  n <- length(returns)
  out <- rep(NA_real_, n)
  if (n <= window) {
    return(out)
  }

  for (t in seq.int(window + 1, n)) {
    past <- returns[seq.int(t - window, t - 1)]
    out[t] <- if (method == "historical") {
      stats::quantile(past, probs = 1 - confidence, names = FALSE)
    } else {
      stats::qnorm(1 - confidence) * stats::sd(past)
    }
  }
  out
}

#' Backtest a VaR series with the Kupiec unconditional coverage test.
#'
#'   LR = -2 * ln[ (1-p)^(n-x) p^x / ((1-pi)^(n-x) pi^x) ]   ~   chi-squared, 1 df
#'
#' where p is the expected breach rate, x the observed breaches and pi = x/n.
#' Too few breaches fails the test as surely as too many: a model that never
#' breaches is not conservative, it is miscalibrated and expensive.
#'
#' @param returns Numeric vector of realised log returns.
#' @param var_series VaR returns from rolling_var(), aligned.
#' @param confidence Confidence level the VaR was estimated at.
#' @param alpha Significance level for the test.
#' @return A one-row data frame: observations, expected and observed breaches,
#'   breach rate, the LR statistic, its p-value and the decision.
backtest_var <- function(returns, var_series, confidence,
                         alpha = params()$var$backtest$alpha) {
  usable <- is.finite(returns) & is.finite(var_series)
  r <- returns[usable]
  v <- var_series[usable]

  n <- length(r)
  breaches <- sum(r < v)
  p <- 1 - confidence
  pi_hat <- breaches / n

  statistic <- if (breaches == 0 || breaches == n) {
    NA_real_
  } else {
    -2 * (
      (n - breaches) * log(1 - p) + breaches * log(p) -
        (n - breaches) * log(1 - pi_hat) - breaches * log(pi_hat)
    )
  }
  p_value <- stats::pchisq(statistic, df = 1, lower.tail = FALSE)

  data.frame(
    confidence = confidence,
    n = n,
    expected_breaches = n * p,
    observed_breaches = breaches,
    breach_rate = pi_hat,
    lr_statistic = statistic,
    p_value = p_value,
    reject_model = !is.na(p_value) & p_value < alpha,
    stringsAsFactors = FALSE
  )
}

#' Assemble a VaR table across confidence levels and horizons.
#'
#' @param returns Numeric vector of log returns.
#' @param value Position or portfolio value.
#' @param sigma Daily volatility for the parametric leg. Defaults to the sample
#'   standard deviation.
#' @param confidence Confidence levels.
#' @param horizons Horizons in days.
#' @return A data frame with one row per method, confidence level and horizon.
var_table <- function(returns, value,
                      sigma = stats::sd(returns[is.finite(returns)]),
                      confidence = params()$var$confidence,
                      horizons = params()$var$horizons_days) {
  grid <- expand.grid(confidence = confidence, horizon = horizons)

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    rbind(
      var_historical(returns, grid$confidence[i], grid$horizon[i], value),
      var_parametric(sigma, grid$confidence[i], grid$horizon[i], value)
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$method, out$confidence, out$horizon), ]
}
