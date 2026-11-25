#' Backtest a VaR series with the Kupiec unconditional coverage test.
#'
#'   LR = -2 * ln[ (1-p)^(n-x) p^x / ((1-pi)^(n-x) pi^x) ]   ~   chi-squared, 1 df
#'
#' where p is the expected breach rate, x the observed breaches and pi = x/n.
#' Too few breaches fails the test as surely as too many: a model that never
#' breaches is not conservative, it is miscalibrated and expensive.
#'
#' @param returns Signed realised returns or P&L.
#' @param var_series Aligned lower thresholds in the same units as returns.
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

  statistic <- if (n == 0L) {
    NA_real_
  } else {
    # dbinom also evaluates the limiting likelihood at x = 0 and x = n.
    max(0, 2 * (stats::dbinom(breaches, n, pi_hat, log = TRUE) -
                  stats::dbinom(breaches, n, p, log = TRUE)))
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
