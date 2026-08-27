# Geometric Brownian motion.
#
# The model:
#
#   dS_t = mu S_t dt + sigma S_t dW_t
#
# whose solution, by Ito's lemma, is
#
#   S_t = S_0 * exp( (mu - sigma^2 / 2) t + sigma W_t )
#
# so that ln S_t is normally distributed and S_t is lognormally distributed.
#
# A note on parameterisation, because this is where the arithmetic usually goes
# wrong. The functions below take `nu`, the drift of the LOG price -- which is
# exactly the sample mean of the log returns, nothing further to adjust:
#
#   ln S_t ~ N( ln S_0 + nu * t ,  sigma^2 * t )
#
# The arithmetic drift mu of the price is then mu = nu + sigma^2 / 2, and it is
# the arithmetic drift that governs the EXPECTED price, while nu governs the
# MEDIAN. Both are reported, because at long horizons they differ enough that
# quoting one as though it were the other is a material error.

#' Estimate GBM parameters from a return series.
#'
#' @param returns Numeric vector of daily log returns.
#' @param trading_days Trading days per year.
#' @return A list with daily and annualised nu (log drift), sigma, and the
#'   implied arithmetic drift mu.
gbm_params <- function(returns,
                       trading_days = params()$conventions$trading_days_per_year) {
  returns <- returns[is.finite(returns)]
  nu <- mean(returns)
  sigma <- stats::sd(returns)

  list(
    nu_daily = nu,
    sigma_daily = sigma,
    mu_daily = nu + sigma^2 / 2,
    nu_annual = annualise_mean(nu, trading_days),
    sigma_annual = annualise_vol(sigma, trading_days),
    mu_annual = annualise_mean(nu, trading_days) + annualise_vol(sigma, trading_days)^2 / 2,
    n = length(returns),
    trading_days = trading_days
  )
}

#' Closed-form GBM projection over one or more horizons.
#'
#' The confidence interval is the lognormal quantile interval, not a symmetric
#' band around the mean: the price cannot go below zero and its distribution is
#' right-skewed, so a symmetric interval would be wrong on both sides.
#'
#'   lower = S_0 * exp( nu*t - z * sigma * sqrt(t) )
#'   upper = S_0 * exp( nu*t + z * sigma * sqrt(t) )
#'
#' @param s0 Initial price.
#' @param nu Drift of the log price, per unit of t.
#' @param sigma Volatility, per unit of sqrt(t).
#' @param horizons Numeric vector of horizons, in the same time unit as nu.
#' @param confidence Confidence level for the interval.
#' @return One row per horizon: expected price, median price, interval bounds
#'   and the interval width as a fraction of the median.
gbm_projection <- function(s0, nu, sigma, horizons,
                           confidence = params()$gbm$confidence) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)

  out <- data.frame(
    horizon = horizons,
    s0 = s0,
    expected = s0 * exp(nu * horizons + (sigma^2 * horizons) / 2),
    median = s0 * exp(nu * horizons),
    lower = s0 * exp(nu * horizons - z * sigma * sqrt(horizons)),
    upper = s0 * exp(nu * horizons + z * sigma * sqrt(horizons)),
    confidence = confidence,
    stringsAsFactors = FALSE
  )
  out$interval_width <- (out$upper - out$lower) / out$median
  out
}

#' Simulate GBM price paths.
#'
#' Uses the exact solution rather than an Euler discretisation, so the result is
#' unbiased at any step size.
#'
#' @param s0 Initial price.
#' @param nu Drift of the log price, per step.
#' @param sigma Volatility, per sqrt(step).
#' @param n_steps Number of steps to simulate.
#' @param n_paths Number of paths.
#' @param seed Optional seed. Defaults to the configured project seed.
#' @return A matrix with n_steps + 1 rows and n_paths columns, starting at s0.
gbm_paths <- function(s0, nu, sigma, n_steps,
                      n_paths = params()$gbm$n_paths,
                      seed = NULL) {
  if (is.null(seed)) {
    use_seed()
  } else {
    set.seed(seed)
  }

  shocks <- matrix(stats::rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  increments <- nu + sigma * shocks
  log_paths <- rbind(0, apply(increments, 2, cumsum))
  s0 * exp(log_paths)
}

#' Terminal-price distribution from simulated paths.
#'
#' A Monte Carlo cross-check on gbm_projection(). If the two disagree by more
#' than simulation error, one of them is wrong.
#'
#' @param paths A matrix from gbm_paths().
#' @param confidence Confidence level.
#' @return A one-row data frame with the simulated mean, median and interval.
gbm_terminal_summary <- function(paths, confidence = params()$gbm$confidence) {
  terminal <- paths[nrow(paths), ]
  tail_prob <- (1 - confidence) / 2

  data.frame(
    n_paths = length(terminal),
    expected = mean(terminal),
    median = stats::median(terminal),
    lower = unname(stats::quantile(terminal, tail_prob)),
    upper = unname(stats::quantile(terminal, 1 - tail_prob)),
    confidence = confidence,
    stringsAsFactors = FALSE
  )
}
