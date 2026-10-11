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
#'   expected = S_0 * exp( mu*t )                       with mu = nu + sigma^2 / 2
#'   variance = S_0^2 * exp( 2*mu*t ) * ( exp(sigma^2 * t) - 1 )
#'   median   = S_0 * exp( nu*t )
#'   mode     = S_0 * exp( (nu - sigma^2) * t )
#'   lower    = S_0 * exp( nu*t - z * sigma * sqrt(t) )
#'   upper    = S_0 * exp( nu*t + z * sigma * sqrt(t) )
#'
#' The same quantities, in the same order, as the class's gbm_summary().
#'
#' @param s0 Initial price.
#' @param nu Drift of the log price, per unit of t.
#' @param sigma Volatility, per unit of sqrt(t).
#' @param horizons Numeric vector of horizons, in the same time unit as nu.
#' @param confidence Confidence level for the interval.
#' @return One row per horizon: expected price, its variance and standard
#'   deviation, median, mode, interval bounds, and the interval width both in
#'   price units and as a fraction of s0.
gbm_projection <- function(s0, nu, sigma, horizons,
                           confidence = params()$gbm$confidence) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  mu <- nu + sigma^2 / 2

  out <- data.frame(
    horizon = horizons,
    s0 = s0,
    expected = s0 * exp(mu * horizons),
    variance = s0^2 * exp(2 * mu * horizons) * (exp(sigma^2 * horizons) - 1),
    median = s0 * exp(nu * horizons),
    mode = s0 * exp((nu - sigma^2) * horizons),
    lower = s0 * exp(nu * horizons - z * sigma * sqrt(horizons)),
    upper = s0 * exp(nu * horizons + z * sigma * sqrt(horizons)),
    confidence = confidence,
    stringsAsFactors = FALSE
  )
  out$sd <- sqrt(out$variance)
  out$interval_width <- out$upper - out$lower
  out$relative_width <- out$interval_width / s0
  out
}

#' Closed-form projection under the local (dS) approximation.
#'
#' Holding the price at S_0 for the whole horizon turns the GBM differential
#' into a normal change in price:
#'
#'   Delta S ~= mu * S_0 * h + sigma * S_0 * sqrt(h) * Z,   Z ~ N(0, 1)
#'
#' so S_h is normal with mean S_0 * (1 + mu * h) and standard deviation
#' sigma * S_0 * sqrt(h), and the interval is symmetric around that mean. mu is
#' the ARITHMETIC drift of the price, nu + sigma^2 / 2, not the log drift.
#'
#' @param s0 Initial price.
#' @param mu Arithmetic drift, per unit of h.
#' @param sigma Volatility, per unit of sqrt(h).
#' @param horizons Numeric vector of horizons, in the same time unit as mu.
#' @param confidence Confidence level for the interval.
#' @return One row per horizon, with the columns of gbm_projection() that the
#'   normal approximation defines.
gbm_local_projection <- function(s0, mu, sigma, horizons,
                                 confidence = params()$gbm$confidence) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  expected <- s0 * (1 + mu * horizons)
  sd <- sigma * s0 * sqrt(horizons)

  out <- data.frame(
    horizon = horizons,
    s0 = s0,
    expected = expected,
    variance = sd^2,
    lower = expected - z * sd,
    upper = expected + z * sd,
    confidence = confidence,
    stringsAsFactors = FALSE
  )
  out$sd <- sd
  out$interval_width <- out$upper - out$lower
  out$relative_width <- out$interval_width / s0
  out
}

#' Simulated prices under the local (dS) approximation.
#'
#'   S_h = S_0 + mu * S_0 * h + sigma * S_0 * sqrt(h) * z
#'
#' @param s0 Initial price.
#' @param mu Arithmetic drift, per unit of h.
#' @param sigma Volatility, per unit of sqrt(h).
#' @param horizon A single horizon.
#' @param z Vector of standard normal draws, shared across horizons so that
#'   differences between horizons are not differences between samples.
#' @return A numeric vector of simulated prices, one per draw.
gbm_local_draws <- function(s0, mu, sigma, horizon, z) {
  s0 + mu * s0 * horizon + sigma * s0 * sqrt(horizon) * z
}

#' Simulated prices from the exact GBM solution.
#'
#'   S_T = S_0 * exp( nu * T + sigma * sqrt(T) * z ),   nu = mu - sigma^2 / 2
#'
#' @param s0 Initial price.
#' @param nu Drift of the log price, per unit of T.
#' @param sigma Volatility, per unit of sqrt(T).
#' @param horizon A single horizon.
#' @param z Vector of standard normal draws.
#' @return A numeric vector of simulated prices, one per draw.
gbm_exact_draws <- function(s0, nu, sigma, horizon, z) {
  s0 * exp(nu * horizon + sigma * sqrt(horizon) * z)
}

#' Monte Carlo check of a closed-form projection.
#'
#' Compares the simulated mean and variance with their formulas and counts the
#' share of draws inside the theoretical interval. A sample proportion has
#' standard error sqrt(p * (1 - p) / n), so at the nominal confidence the
#' coverage should sit within a few of those errors of it.
#'
#' @param draws Simulated prices at one horizon.
#' @param theory One row of gbm_projection() or gbm_local_projection().
#' @return A one-row data frame: simulated and theoretical mean and variance,
#'   their relative gaps, the coverage and its Monte Carlo error.
gbm_mc_check <- function(draws, theory) {
  stopifnot(nrow(theory) == 1)
  n <- length(draws)
  coverage <- mean(draws >= theory$lower & draws <= theory$upper)

  data.frame(
    horizon = theory$horizon,
    n_simulations = n,
    mean_sim = mean(draws),
    mean_theory = theory$expected,
    mean_gap = mean(draws) / theory$expected - 1,
    var_sim = stats::var(draws),
    var_theory = theory$variance,
    var_gap = stats::var(draws) / theory$variance - 1,
    coverage = coverage,
    coverage_mc_error = sqrt(theory$confidence * (1 - theory$confidence) / n),
    stringsAsFactors = FALSE
  )
}

#' Euler-Maruyama trajectories of the GBM.
#'
#'   S_{t+dt} = S_t + mu * S_t * dt + sigma * S_t * sqrt(dt) * Z_t
#'
#' Each step applies the local approximation from the price reached so far, so
#' the error that gbm_local_projection() makes over the whole horizon is made
#' here over one step only.
#'
#' @param s0 Initial price.
#' @param mu Arithmetic drift, per unit of dt.
#' @param sigma Volatility, per unit of sqrt(dt).
#' @param z A matrix of standard normal draws, one row per step and one column
#'   per trajectory.
#' @param dt Step length, in the time unit of mu.
#' @return A matrix with nrow(z) + 1 rows, the first equal to s0.
gbm_euler_paths <- function(s0, mu, sigma, z, dt = 1) {
  z <- as.matrix(z)
  paths <- matrix(NA_real_, nrow = nrow(z) + 1, ncol = ncol(z))
  paths[1, ] <- s0
  for (i in seq_len(nrow(z))) {
    paths[i + 1, ] <- paths[i, ] + mu * paths[i, ] * dt +
      sigma * paths[i, ] * sqrt(dt) * z[i, ]
  }
  paths
}

#' Probability that the price ends below its starting value.
#'
#'   P(S_T < S_0) = Phi( -nu * sqrt(T) / sigma )
#'
#' @param nu Drift of the log price, per unit of T.
#' @param sigma Volatility, per unit of sqrt(T).
#' @param horizons Numeric vector of positive horizons.
#' @return A numeric vector of probabilities.
gbm_loss_probability <- function(nu, sigma, horizons) {
  stats::pnorm(-nu * sqrt(horizons) / sigma)
}

#' Where a realised price falls in the model's distribution.
#'
#'   z = ( ln(S_obs / S_0) - nu * T ) / ( sigma * sqrt(T) )
#'
#' Under the model z is a standard normal draw, so |z| > 1.96 is a 95 %
#' interval miss and its sign says on which side.
#'
#' @param s0 Initial price.
#' @param observed Realised price at the horizon.
#' @param nu Drift of the log price, per unit of T.
#' @param sigma Volatility, per unit of sqrt(T).
#' @param horizon Horizon, in the time unit of nu.
#' @return The standardised outcome.
gbm_standardised_outcome <- function(s0, observed, nu, sigma, horizon) {
  (log(observed / s0) - nu * horizon) / (sigma * sqrt(horizon))
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
