# Conditional variance models.
#
# Four estimators of the same quantity, ordered by how much structure they
# assume:
#
#   MA(m)        sigma^2_t = (1/m) * sum_{i=1..m} r^2_{t-i}
#   EWMA         sigma^2_t = lambda * sigma^2_{t-1} + (1 - lambda) * r^2_{t-1}
#   EWMA, fitted the same recursion with lambda estimated from the data
#   GARCH(p,q)   sigma^2_t = omega + sum alpha_i r^2_{t-i} + sum beta_j sigma^2_{t-j}
#
# All four estimate variance from squared returns rather than from deviations
# around a rolling mean. At daily frequency the mean return is indistinguishable
# from zero -- stage 03 tests exactly that -- and subtracting a noisy estimate of
# it adds variance to the estimator without removing bias.
#
# Every estimate at time t uses information up to t-1 only. A volatility model
# that peeks at the contemporaneous return will look excellent and forecast
# nothing.

#' Moving-average volatility.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param m Window length in observations.
#' @return A numeric vector the same length as returns. The first m entries are
#'   NA, having no full window behind them.
ma_volatility <- function(returns, m) {
  n <- length(returns)
  squared <- returns^2
  sigma2 <- rep(NA_real_, n)
  if (n <= m) {
    return(sqrt(sigma2))
  }
  for (t in seq.int(m + 1, n)) {
    sigma2[t] <- mean(squared[seq.int(t - m, t - 1)])
  }
  sqrt(sigma2)
}

#' Moving-average volatility across a grid of windows.
#'
#' @param returns Numeric vector of log returns.
#' @param windows Window lengths to sweep.
#' @return A matrix, one column per window.
ma_volatility_grid <- function(returns, windows = params()$variance$ma_windows) {
  out <- vapply(windows, function(m) ma_volatility(returns, m), numeric(length(returns)))
  colnames(out) <- paste0("m_", windows)
  out
}

#' EWMA volatility.
#'
#' The recursion is seeded with the sample variance of the first
#' `burn_in` observations, which is the usual convention and makes the series
#' insensitive to the arbitrary choice of a single starting value.
#'
#' @param returns Numeric vector of log returns.
#' @param lambda Decay factor in (0, 1).
#' @param burn_in Observations used to seed the recursion.
#' @return A numeric vector of conditional volatilities, aligned with returns.
ewma_volatility <- function(returns,
                            lambda = params()$variance$ewma$lambda_riskmetrics,
                            burn_in = 20) {
  stopifnot(lambda > 0, lambda < 1)
  n <- length(returns)
  sigma2 <- rep(NA_real_, n)
  if (n <= burn_in) {
    return(sqrt(sigma2))
  }

  sigma2[burn_in] <- stats::var(returns[seq_len(burn_in)])
  for (t in seq.int(burn_in + 1, n)) {
    sigma2[t] <- lambda * sigma2[t - 1] + (1 - lambda) * returns[t - 1]^2
  }
  sqrt(sigma2)
}

#' Select the EWMA decay factor from the data.
#'
#' Two loss functions, because they answer different questions and can disagree:
#'
#'   "rmse"    minimises squared error against realised variance. Symmetric, and
#'             therefore dominated by the largest observations.
#'   "loglik"  maximises the Gaussian log-likelihood of the returns given the
#'             conditional variance. Penalises underestimating volatility far
#'             more heavily than overestimating it, which is the asymmetry a
#'             risk manager actually faces.
#'
#' @param returns Numeric vector of log returns.
#' @param grid Candidate lambda values.
#' @param loss "rmse" or "loglik".
#' @return A list with the selected lambda, the loss surface and the loss used.
ewma_fit_lambda <- function(returns,
                            grid = ewma_lambda_grid(),
                            loss = c("rmse", "loglik")) {
  loss <- match.arg(loss)

  scores <- vapply(grid, function(lambda) {
    sigma <- ewma_volatility(returns, lambda = lambda)
    usable <- is.finite(sigma) & is.finite(returns) & sigma > 0
    if (sum(usable) < 30) {
      return(NA_real_)
    }
    if (loss == "rmse") {
      sqrt(mean((sigma[usable]^2 - returns[usable]^2)^2))
    } else {
      # Negated so that both criteria are minimised.
      -sum(stats::dnorm(returns[usable], mean = 0, sd = sigma[usable], log = TRUE))
    }
  }, numeric(1))

  surface <- data.frame(lambda = grid, score = scores, stringsAsFactors = FALSE)
  best <- surface$lambda[which.min(surface$score)]

  list(
    lambda = best,
    alpha = 1 - best,
    loss = loss,
    surface = surface
  )
}

#' Fit an ARCH/GARCH model, selecting orders on an information criterion.
#'
#' Sweeps (p, q) up to the configured maximum across the configured error
#' distributions and returns the best fit. The sweep is reported in full, not
#' just the winner: a selection table is evidence, a single chosen order is an
#' assertion.
#'
#' @param returns Numeric vector of log returns.
#' @param max_p Maximum GARCH order.
#' @param max_q Maximum ARCH order.
#' @param distributions Conditional distributions to try.
#' @param criterion "AIC" or "BIC".
#' @return A list with the fitted model, its specification and the full
#'   selection table.
fit_garch <- function(returns,
                      max_p = params()$variance$garch$max_p,
                      max_q = params()$variance$garch$max_q,
                      distributions = params()$variance$garch$distributions,
                      criterion = c("AIC", "BIC")) {
  criterion <- match.arg(criterion)
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package 'rugarch' is required. Run renv::restore().", call. = FALSE)
  }

  index <- if (criterion == "AIC") 1L else 2L
  candidates <- expand.grid(
    p = seq_len(max_p),
    q = seq_len(max_q),
    distribution = distributions,
    stringsAsFactors = FALSE
  )

  fits <- vector("list", nrow(candidates))
  scores <- rep(NA_real_, nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    spec <- rugarch::ugarchspec(
      variance.model = list(
        model = "sGARCH",
        garchOrder = c(candidates$q[i], candidates$p[i])
      ),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = candidates$distribution[i]
    )
    fit <- tryCatch(
      rugarch::ugarchfit(spec, data = returns, solver = "hybrid"),
      error = function(e) NULL
    )
    if (is.null(fit) || fit@fit$convergence != 0) {
      next
    }
    fits[[i]] <- fit
    scores[i] <- rugarch::infocriteria(fit)[index]
  }

  if (all(is.na(scores))) {
    stop("No GARCH specification converged.", call. = FALSE)
  }

  selection <- cbind(candidates, score = scores)
  names(selection)[names(selection) == "score"] <- tolower(criterion)
  best <- which.min(scores)

  list(
    fit = fits[[best]],
    order = c(p = candidates$p[best], q = candidates$q[best]),
    distribution = candidates$distribution[best],
    criterion = criterion,
    selection = selection[order(scores), ]
  )
}

#' Conditional volatility from a fitted GARCH model.
#'
#' @param garch A list returned by fit_garch().
#' @return A numeric vector of in-sample conditional volatilities.
garch_volatility <- function(garch) {
  as.numeric(rugarch::sigma(garch$fit))
}

#' Roll a fitted GARCH model forward one step at a time.
#'
#' Refits nothing: parameters are those estimated in sample, and only the
#' conditional variance recursion is advanced with realised out-of-sample
#' returns. That is the honest way to ask what the model would have said in
#' January 2026 knowing only what was available on 31 December 2025.
#'
#' @param garch A list returned by fit_garch().
#' @param oos_returns Out-of-sample returns, in time order.
#' @return A numeric vector of one-step-ahead volatility forecasts.
garch_forecast_path <- function(garch, oos_returns) {
  coefficients <- rugarch::coef(garch$fit)
  omega <- coefficients[["omega"]]
  alpha <- coefficients[grepl("^alpha", names(coefficients))]
  beta <- coefficients[grepl("^beta", names(coefficients))]

  in_sample_sigma <- garch_volatility(garch)
  in_sample_returns <- as.numeric(garch$fit@fit$data)

  sigma2 <- c(tail(in_sample_sigma, length(beta))^2)
  shocks <- c(tail(in_sample_returns, length(alpha))^2)

  forecasts <- numeric(length(oos_returns))
  for (t in seq_along(oos_returns)) {
    next_sigma2 <- omega + sum(alpha * shocks) + sum(beta * sigma2)
    forecasts[t] <- sqrt(next_sigma2)
    shocks <- c(oos_returns[t]^2, utils::head(shocks, -1))
    sigma2 <- c(next_sigma2, utils::head(sigma2, -1))
  }
  forecasts
}

#' Score volatility forecasts against a realised measure.
#'
#' QLIKE is included alongside RMSE and MAE because it is robust to the fact
#' that realised volatility is itself a noisy proxy for the latent quantity,
#' and because it penalises underestimation asymmetrically.
#'
#' @param forecast Numeric vector of forecast volatilities.
#' @param realised Numeric vector of realised volatilities, aligned.
#' @param label Model name, carried into the output.
#' @return A one-row data frame: n, RMSE, MAE, QLIKE and mean bias.
score_volatility <- function(forecast, realised, label = NA_character_) {
  usable <- is.finite(forecast) & is.finite(realised) & forecast > 0
  f <- forecast[usable]
  r <- realised[usable]

  data.frame(
    model = label,
    n = length(f),
    rmse = sqrt(mean((f - r)^2)),
    mae = mean(abs(f - r)),
    qlike = mean(log(f^2) + (r^2) / (f^2)),
    bias = mean(f - r),
    stringsAsFactors = FALSE
  )
}

#' Realised volatility proxy.
#'
#' The absolute return is a one-observation estimate of the day's volatility.
#' It is extremely noisy but unbiased, which is what makes it usable as a
#' scoring reference across models.
#'
#' @param returns Numeric vector of log returns.
#' @return A numeric vector.
realised_volatility <- function(returns) {
  abs(returns)
}
