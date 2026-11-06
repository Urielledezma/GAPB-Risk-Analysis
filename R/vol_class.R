# Variance models as taught in class (Modulo 3), for stage 04.
#
#   EWMA     sigma^2_t = (1 - lambda) r^2_{t-1} + lambda sigma^2_{t-1},
#            lambda by maximum likelihood (Verosimilitud.qmd)
#   DDEWMA   sigma_t = alpha z_{t-1} + (1 - alpha) sigma_{t-1},
#            z_t = |r_t - rbar| / rho, alpha by least squares (DDEWMA.Rmd)
#   GARCH    ARMA mean from auto.arima, ARCH-LM test, six sGARCH candidates
#            with normal errors, chosen on significance, likelihood, AIC/BIC
#            and parsimony (ARCH - GARCH.qmd, Funciones_GARCH.R)
#
# The moving average lives in vol_models.R with the rest of the class
# procedure for choosing m.
#
# Every model is fitted on the first n_fit returns of a series and then run
# forward over the whole series with those parameters frozen. Entry t of a
# path is the forecast for session t made with returns up to t - 1, so the
# part after n_fit is out of sample: it sees the data as they arrive but never
# re-estimates anything with them.

# --- Shared --------------------------------------------------------------------

#' Half-life of a geometric decay, in sessions.
#'
#' A shock's weight falls by the factor `decay` each session, so it halves
#' after ln(0.5) / ln(decay) sessions: lambda for EWMA, 1 - alpha for DDEWMA,
#' alpha + beta for GARCH(1,1).
#'
#' @param decay Per-session decay factor in (0, 1).
#' @return The half-life in sessions.
decay_half_life <- function(decay) {
  stopifnot(all(decay > 0), all(decay < 1))
  log(0.5) / log(decay)
}

# --- EWMA by maximum likelihood ---------------------------------------------------

#' EWMA variance with the class recursion.
#'
#' The recursion is seeded with the first squared return, as in class:
#'
#'   sigma^2_1 = r^2_1,   sigma^2_t = (1 - lambda) r^2_{t-1} + lambda sigma^2_{t-1}.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param lambda Decay factor in (0, 1).
#' @return Variances aligned with returns; entry t uses returns up to t - 1.
ewma_class_variance <- function(returns, lambda) {
  stopifnot(length(returns) >= 2, lambda > 0, lambda < 1)
  sigma2 <- numeric(length(returns))
  sigma2[1] <- returns[1]^2
  for (t in seq.int(2, length(returns))) {
    sigma2[t] <- (1 - lambda) * returns[t - 1]^2 + lambda * sigma2[t - 1]
  }
  sigma2
}

#' Gaussian log-likelihood of the EWMA variances, as written in class.
#'
#'   l(lambda) = sum_t ( -ln sigma^2_t - r^2_t / sigma^2_t ),
#'
#' the normal log-density without its constants.
#'
#' @param returns Numeric vector of log returns.
#' @param lambda Decay factor.
#' @return The log-likelihood.
ewma_class_loglik <- function(returns, lambda) {
  if (returns[1] == 0) {
    stop("ewma_class_loglik: the first return is zero, so the class seed sigma^2_1 = r^2_1 ",
         "is zero and the likelihood is infinite. Start the sample one session later.",
         call. = FALSE)
  }
  sigma2 <- ewma_class_variance(returns, lambda)
  sum(-log(sigma2) - returns^2 / sigma2)
}

#' Maximum-likelihood lambda, searched with optimize() as in class.
#'
#' @param returns Numeric vector of log returns.
#' @param interval Search interval for lambda.
#' @return A list: lambda and loglik.
ewma_class_fit <- function(returns, interval = params()$variance$ewma$lambda_interval) {
  best <- stats::optimize(function(lambda) ewma_class_loglik(returns, lambda),
                          interval = unlist(interval), maximum = TRUE)
  list(lambda = best$maximum, loglik = best$objective)
}

#' The class iteration made explicit: the log-likelihood over a grid of lambdas.
#'
#' @param returns Numeric vector of log returns.
#' @param grid Candidate lambdas.
#' @return A data frame: lambda and loglik.
ewma_loglik_profile <- function(returns, grid = ewma_lambda_grid()) {
  data.frame(lambda = grid,
             loglik = vapply(grid, function(l) ewma_class_loglik(returns, l), numeric(1)))
}

#' Fit EWMA on the first n_fit returns and run it over the whole series.
#'
#' @param returns Numeric vector of log returns.
#' @param n_fit Number of leading returns the parameter may see.
#' @return A list: lambda, loglik, sigma (aligned with returns) and next_sigma,
#'   the forecast for the session after the last return.
ewma_class_path <- function(returns, n_fit = length(returns)) {
  stopifnot(n_fit >= 2, n_fit <= length(returns))
  fit <- ewma_class_fit(returns[seq_len(n_fit)])
  sigma2 <- ewma_class_variance(returns, fit$lambda)
  n <- length(returns)
  next_sigma2 <- (1 - fit$lambda) * returns[n]^2 + fit$lambda * sigma2[n]
  c(fit, list(sigma = sqrt(sigma2), next_sigma = sqrt(next_sigma2)))
}

# --- Data-driven EWMA ------------------------------------------------------------------

#' Centre and scale of the data-driven EWMA.
#'
#' rho = cor(r - rbar, sign(r - rbar)). Because sign(r - rbar) has mean near
#' zero and unit variance, rho is E|r - rbar| / sigma: about sqrt(2 / pi) = 0.80
#' for a normal, lower when the tails are heavy. z_t = |r_t - rbar| / rho is
#' then a one-observation estimate of sigma on the volatility scale.
#'
#' @param returns Numeric vector of log returns.
#' @return A list: mean and rho.
ddewma_scale <- function(returns) {
  centre <- mean(returns)
  deviations <- returns - centre
  list(mean = centre, rho = stats::cor(deviations, sign(deviations)))
}

#' Data-driven EWMA volatility with the class recursion.
#'
#'   sigma_1 = z_1,   sigma_t = alpha z_{t-1} + (1 - alpha) sigma_{t-1}.
#'
#' alpha weighs the newest observation, so it plays the role of 1 - lambda.
#'
#' @param returns Numeric vector of log returns.
#' @param alpha Smoothing weight in (0, 1).
#' @param scale A list with mean and rho, from ddewma_scale().
#' @return Volatilities aligned with returns; entry t uses returns up to t - 1.
ddewma_volatility <- function(returns, alpha, scale) {
  stopifnot(length(returns) >= 2, alpha > 0, alpha < 1, scale$rho > 0)
  z <- abs(returns - scale$mean) / scale$rho
  sigma <- numeric(length(z))
  sigma[1] <- z[1]
  for (t in seq.int(2, length(z))) {
    sigma[t] <- alpha * z[t - 1] + (1 - alpha) * sigma[t - 1]
  }
  sigma
}

#' Sum of squared errors of the DDEWMA against z, the class objective.
#'
#' @param returns Numeric vector of log returns.
#' @param alpha Smoothing weight.
#' @param scale A list with mean and rho.
#' @return The sum of squared errors.
ddewma_sse <- function(returns, alpha, scale) {
  z <- abs(returns - scale$mean) / scale$rho
  sum((z - ddewma_volatility(returns, alpha, scale))^2)
}

#' Least-squares alpha, searched with optimize() as in class.
#'
#' @param returns Numeric vector of log returns.
#' @param interval Search interval for alpha.
#' @return A list: alpha, sse, mean and rho.
ddewma_fit <- function(returns, interval = params()$variance$ddewma$alpha_interval) {
  scale <- ddewma_scale(returns)
  best <- stats::optimize(function(alpha) ddewma_sse(returns, alpha, scale),
                          interval = unlist(interval))
  list(alpha = best$minimum, sse = best$objective, mean = scale$mean, rho = scale$rho)
}

#' Fit the DDEWMA on the first n_fit returns and run it over the whole series.
#'
#' The centre and rho are estimated on the same n_fit returns as alpha, so no
#' later observation leaks into the scale.
#'
#' @param returns Numeric vector of log returns.
#' @param n_fit Number of leading returns the parameters may see.
#' @return A list: alpha, sse, mean, rho, sigma (aligned with returns) and
#'   next_sigma.
ddewma_path <- function(returns, n_fit = length(returns)) {
  stopifnot(n_fit >= 2, n_fit <= length(returns))
  fit <- ddewma_fit(returns[seq_len(n_fit)])
  sigma <- ddewma_volatility(returns, fit$alpha, fit)
  n <- length(returns)
  z_last <- abs(returns[n] - fit$mean) / fit$rho
  c(fit, list(sigma = sigma, next_sigma = fit$alpha * z_last + (1 - fit$alpha) * sigma[n]))
}

# --- ARCH / GARCH -----------------------------------------------------------------------

#' Engle's ARCH-LM test, the class implementation.
#'
#' Regresses x_t^2 on its first `lags` lags; T * R^2 is chi-squared with `lags`
#' degrees of freedom under no ARCH effects. p < alpha: evidence of remaining
#' ARCH structure.
#'
#' @param x Residuals or returns.
#' @param lags Number of lags.
#' @param demean Subtract the mean before squaring.
#' @return A list: statistic, df and p_value.
arch_lm_test <- function(x, lags = params()$variance$garch$arch_lm_lags, demean = FALSE) {
  x <- as.numeric(stats::na.omit(as.numeric(x)))
  if (demean) {
    x <- x - mean(x)
  }
  lagged <- stats::embed(x^2, lags + 1)
  aux <- stats::lm(lagged[, 1] ~ lagged[, -1])
  statistic <- length(stats::residuals(aux)) * summary(aux)$r.squared
  list(statistic = statistic, df = lags,
       p_value = stats::pchisq(statistic, df = lags, lower.tail = FALSE))
}

#' ARMA order of the mean equation, chosen with auto.arima as in class.
#'
#' @param returns Numeric vector of log returns.
#' @return A list: order, a named vector c(p, q), and fit, the auto.arima model.
arma_mean_order <- function(returns) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required. Run renv::restore().", call. = FALSE)
  }
  fit <- forecast::auto.arima(as.numeric(returns), d = 0, stationary = TRUE, seasonal = FALSE)
  order <- forecast::arimaorder(fit)
  list(order = c(p = unname(order["p"]), q = unname(order["q"])), fit = fit)
}

#' The six ARCH/GARCH candidates of the class notebook.
#'
#' @param orders A list of c(ARCH order, GARCH order) pairs.
#' @return A data frame: model, p (ARCH order) and q (GARCH order).
garch_class_candidates <- function(orders = params()$variance$garch$candidates) {
  p <- vapply(orders, function(o) as.numeric(o[[1]]), numeric(1))
  q <- vapply(orders, function(o) as.numeric(o[[2]]), numeric(1))
  model <- ifelse(q == 0, paste0("ARCH(", p, ")"), paste0("GARCH(", p, ",", q, ")"))
  data.frame(model = model, p = p, q = q, stringsAsFactors = FALSE)
}

#' Class specification: sGARCH(p, q) variance, ARMA mean with a constant.
#'
#' p is the ARCH order and q the GARCH order, rugarch's garchOrder = c(p, q).
#'
#' @param p ARCH order.
#' @param q GARCH order.
#' @param arma_order c(AR order, MA order) of the mean.
#' @param distribution Conditional distribution.
#' @return A uGARCHspec.
garch_class_spec <- function(p, q, arma_order,
                             distribution = params()$variance$garch$distribution) {
  rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
    mean.model = list(armaOrder = as.numeric(arma_order), include.mean = TRUE),
    distribution.model = distribution
  )
}

#' Starting points for the GARCH likelihood search.
#'
#' The GARCH likelihood of a series with a few extreme days can have more than
#' one local maximum -- a short-memory one, high alpha and low beta, and a
#' long-memory one with alpha near zero and beta near one -- and a single
#' search from rugarch's default start can stop at the lower of the two.
#'
#' @param alphas Starting sums of the ARCH coefficients.
#' @param betas Starting sums of the GARCH coefficients.
#' @return A data frame of (alpha, beta) starting pairs, all with alpha + beta < 1.
garch_class_starts <- function(alphas = params()$variance$garch$start_alpha,
                               betas = params()$variance$garch$start_beta) {
  starts <- expand.grid(alpha = unlist(alphas), beta = unlist(betas))
  stopifnot(all(starts$alpha + starts$beta < 1))
  starts
}

#' Fit a class specification on the first n_fit returns.
#'
#' Uses ugarchfit's out.sample, as the class notebook does, so the returns
#' after n_fit stay in the object for the rolling forecast but never enter the
#' likelihood. The estimator is the class one, maximum likelihood with the
#' hybrid solver; it is only run from rugarch's default start and from every
#' pair in `starts`, and the fit with the highest likelihood is kept.
#'
#' @param returns Numeric vector of log returns.
#' @param p ARCH order.
#' @param q GARCH order.
#' @param arma_order c(AR order, MA order).
#' @param n_fit Number of leading returns the parameters may see.
#' @param starts A data frame of (alpha, beta) starting pairs, or NULL for the
#'   default start only.
#' @return The uGARCHfit with the highest likelihood among the searches that
#'   converged, or NULL if none did.
garch_class_fit <- function(returns, p, q, arma_order, n_fit = length(returns),
                            starts = garch_class_starts()) {
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package 'rugarch' is required. Run renv::restore().", call. = FALSE)
  }
  stopifnot(n_fit <= length(returns))
  data <- as.numeric(returns)
  sample_var <- stats::var(data[seq_len(n_fit)])
  if (!is.null(starts) && q == 0) {
    starts <- data.frame(alpha = unique(starts$alpha), beta = 0)
  }

  best <- NULL
  for (i in c(0, seq_len(if (is.null(starts)) 0 else nrow(starts)))) {
    spec <- garch_class_spec(p, q, arma_order)
    if (i > 0) {
      a <- starts$alpha[i]
      b <- if (q > 0) starts$beta[i] else 0
      values <- list(omega = sample_var * (1 - a - b))
      values[paste0("alpha", seq_len(p))] <- a / p
      if (q > 0) {
        values[paste0("beta", seq_len(q))] <- b / q
      }
      spec <- rugarch::`setstart<-`(spec, value = values)
    }
    fit <- tryCatch(
      rugarch::ugarchfit(spec, data = data, out.sample = length(data) - n_fit,
                         solver = params()$variance$garch$solver),
      error = function(e) NULL
    )
    if (is.null(fit) || fit@fit$convergence != 0) {
      next
    }
    if (is.null(best) || rugarch::likelihood(fit) > rugarch::likelihood(best)) {
      best <- fit
    }
  }
  best
}

#' One row of the class selection table.
#'
#' The class summary -- log-likelihood, number of parameters k, AIC = -2 l + 2k,
#' BIC = -2 l + ln(n) k with n the fitting sample, omega, the sums of the ARCH
#' and GARCH coefficients, their persistence and the long-run variance -- plus
#' the largest p-value among the ARCH and GARCH coefficients, because the class
#' discards a model whose terms lose significance.
#'
#' @param name Model label.
#' @param fit A uGARCHfit or NULL.
#' @param n_fit Size of the fitting sample.
#' @param alpha Significance level.
#' @return A one-row data frame.
garch_class_summary <- function(name, fit, n_fit, alpha = params()$tests$alpha) {
  empty <- data.frame(model = name, converged = FALSE, loglik = NA_real_, k = NA_real_,
                      aic = NA_real_, bic = NA_real_, omega = NA_real_, alpha_sum = NA_real_,
                      beta_sum = NA_real_, persistence = NA_real_,
                      long_run_variance = NA_real_, max_p_value = NA_real_,
                      all_significant = FALSE, stringsAsFactors = FALSE)
  if (is.null(fit)) {
    return(empty)
  }
  cf <- rugarch::coef(fit)
  ll <- as.numeric(rugarch::likelihood(fit))
  k <- length(cf)
  alpha_sum <- sum(cf[grepl("^alpha", names(cf))])
  beta_sum <- sum(cf[grepl("^beta", names(cf))])
  persistence <- alpha_sum + beta_sum
  matcoef <- fit@fit$matcoef
  terms <- grepl("^(alpha|beta)", rownames(matcoef))
  # A constant-variance benchmark, garchOrder = c(0, 0), has no such terms.
  max_p <- if (any(terms)) max(matcoef[terms, 4]) else NA_real_

  empty$converged <- fit@fit$convergence == 0
  empty$loglik <- ll
  empty$k <- k
  empty$aic <- -2 * ll + 2 * k
  empty$bic <- -2 * ll + log(n_fit) * k
  empty$omega <- unname(cf["omega"])
  empty$alpha_sum <- alpha_sum
  empty$beta_sum <- beta_sum
  empty$persistence <- persistence
  empty$long_run_variance <- if (persistence < 1) unname(cf["omega"]) / (1 - persistence) else NA
  empty$max_p_value <- max_p
  empty$all_significant <- is.finite(max_p) && max_p < alpha
  empty
}

#' The constant-variance benchmark, as a row of the selection table.
#'
#' The class mean model, auto.arima's ARMA with constant variance, is the null
#' hypothesis of the ARCH-LM test. Its log-likelihood, with k counting the ARMA
#' coefficients plus the variance, goes through the same AIC and BIC formulas
#' as the GARCH candidates. arima's likelihood treats the first observations
#' exactly where rugarch conditions on them, so the comparison is approximate
#' at the level of a point or so of log-likelihood.
#'
#' @param arma_fit The auto.arima fit from arma_mean_order().
#' @param n_fit Size of the fitting sample.
#' @param name Row label.
#' @return A one-row data frame shaped like garch_class_summary().
arma_benchmark_summary <- function(arma_fit, n_fit, name) {
  row <- garch_class_summary(name, NULL, n_fit)
  ll <- as.numeric(stats::logLik(arma_fit))
  k <- length(stats::coef(arma_fit)) + 1
  row$converged <- TRUE
  row$loglik <- ll
  row$k <- k
  row$aic <- -2 * ll + 2 * k
  row$bic <- -2 * ll + log(n_fit) * k
  row$persistence <- 0
  row$long_run_variance <- arma_fit$sigma2
  row
}

#' Choose among the candidates with the class criteria.
#'
#' A model is eligible if it converged, every ARCH and GARCH coefficient is
#' significant and its persistence is below one, so that a long-run variance
#' exists. Among the eligible, the lowest BIC wins: BIC rewards likelihood and
#' charges for every extra parameter, which is the class's "verosimilitud ...
#' y al final parsimonia" in one number.
#'
#' @param summary A data frame of garch_class_summary() rows.
#' @return The label of the chosen model.
garch_class_select <- function(summary) {
  eligible <- summary$converged & summary$all_significant &
    !is.na(summary$persistence) & summary$persistence < 1
  if (!any(eligible)) {
    stop("garch_class_select: no candidate is significant, convergent and stationary.",
         call. = FALSE)
  }
  pool <- summary[eligible, , drop = FALSE]
  pool$model[which.min(pool$bic)]
}

#' Fit a class GARCH on the first n_fit returns and run it over the series.
#'
#' The in-sample part is rugarch's fitted sigma; the rest comes from
#' ugarchforecast(n.ahead = 1, n.roll = n - n_fit), the class rolling forecast,
#' whose last value is the forecast for the session after the last return.
#'
#' @param returns Numeric vector of log returns.
#' @param p ARCH order.
#' @param q GARCH order.
#' @param arma_order c(AR order, MA order).
#' @param n_fit Number of leading returns the parameters may see.
#' @param starts Starting pairs, passed to garch_class_fit().
#' @return A list: fit, sigma (aligned with returns) and next_sigma.
garch_class_path <- function(returns, p, q, arma_order, n_fit = length(returns),
                             starts = garch_class_starts()) {
  fit <- garch_class_fit(returns, p, q, arma_order, n_fit, starts)
  if (is.null(fit)) {
    stop("garch_class_path: the GARCH(", p, ",", q, ") fit failed.", call. = FALSE)
  }
  n_out <- length(returns) - n_fit
  rolled <- as.numeric(rugarch::sigma(rugarch::ugarchforecast(fit, n.ahead = 1, n.roll = n_out)))
  sigma <- c(as.numeric(rugarch::sigma(fit)), utils::head(rolled, n_out))
  list(fit = fit, sigma = sigma, next_sigma = utils::tail(rolled, 1))
}

#' Long-run level and persistence of a fitted GARCH.
#'
#' @param fit A uGARCHfit.
#' @return A list: persistence, half_life, long_run_variance, long_run_vol.
garch_long_run <- function(fit) {
  cf <- rugarch::coef(fit)
  persistence <- sum(cf[grepl("^(alpha|beta)", names(cf))])
  stopifnot(persistence < 1)
  variance <- unname(cf["omega"]) / (1 - persistence)
  list(persistence = persistence, half_life = decay_half_life(persistence),
       long_run_variance = variance, long_run_vol = sqrt(variance))
}

# --- Comparing models by likelihood -------------------------------------------------

#' Gaussian log-likelihood of residuals given a volatility path.
#'
#'   l = -1/2 sum_t ( ln 2 pi + ln sigma_t^2 + e_t^2 / sigma_t^2 ).
#'
#' It is the class EWMA likelihood, sum(-ln sigma^2 - r^2 / sigma^2), halved
#' and with its constant restored, l = l_class / 2 - (n / 2) ln 2 pi, which is
#' the scale rugarch reports and the one the class AIC and BIC formulas take.
#'
#' @param residuals e_t: the return minus the model's mean.
#' @param sigma Volatility forecasts aligned with residuals.
#' @return The log-likelihood.
gaussian_loglik <- function(residuals, sigma) {
  stopifnot(length(residuals) == length(sigma), all(is.finite(residuals)),
            all(is.finite(sigma)), all(sigma > 0))
  -0.5 * sum(log(2 * pi) + log(sigma^2) + residuals^2 / sigma^2)
}

#' Log-likelihood, AIC and BIC of several volatility models on one sample.
#'
#' Every model is scored on the same sessions, so the likelihoods are
#' comparable, with AIC = -2 l + 2k and BIC = -2 l + k ln n as in class.
#'
#' @param residuals Named list of residual series, one per model, aligned.
#' @param sigmas Named list of volatility paths with the same names.
#' @param k Named vector of parameter counts with the same names.
#' @param sessions Indices of the sessions to score.
#' @return One row per model: model, n, k, loglik, aic and bic.
model_information <- function(residuals, sigmas, k, sessions) {
  models <- names(sigmas)
  stopifnot(setequal(models, names(residuals)), setequal(models, names(k)))
  n <- length(sessions)
  loglik <- vapply(models, function(m) {
    gaussian_loglik(residuals[[m]][sessions], sigmas[[m]][sessions])
  }, numeric(1))
  data.frame(model = models, n = n, k = unname(k[models]), loglik = unname(loglik),
             aic = unname(-2 * loglik + 2 * k[models]),
             bic = unname(-2 * loglik + log(n) * k[models]), stringsAsFactors = FALSE)
}

#' Parametric VaR of a single position, the class formula.
#'
#'   VaR_c = |Z_c| V_0 sigma,
#'
#' with Z_c the normal quantile of 1 - confidence (VaR_Par_Acc_Est.Rmd),
#' reported as a positive loss.
#'
#' @param sigma Daily volatility.
#' @param value Position value V_0.
#' @param confidence Confidence level c.
#' @return The one-day VaR in the currency of value.
class_var <- function(sigma, value, confidence) {
  stopifnot(confidence > 0.5, confidence < 1)
  abs(stats::qnorm(1 - confidence)) * value * sigma
}

# --- Scoring -----------------------------------------------------------------------------

#' Score a volatility path against the realised returns.
#'
#' The four class losses on the variance scale (ma_class_losses()), the QLIKE
#' of the ARCH - GARCH notebook, mean(ln f + y / f), and four descriptive
#' measures: the bias ratio mean(sigma^2) / mean(r^2), above one when the model
#' overestimates; the RMSE and the mean error of sigma against |r|, the
#' reference the assignment names; and the stability, mean |diff(ln sigma)|,
#' the typical relative change of the estimate from one session to the next.
#'
#' @param returns Realised log returns.
#' @param sigma Volatility forecasts aligned with returns.
#' @return A named numeric vector.
forecast_scorecard <- function(returns, sigma) {
  stopifnot(length(returns) == length(sigma), all(is.finite(sigma)), all(sigma > 0))
  realised <- returns^2
  forecast <- sigma^2
  c(ma_class_losses(realised, forecast),
    qlike_log = mean(log(forecast) + realised / forecast),
    bias_ratio = mean(forecast) / mean(realised),
    vol_rmse = sqrt(mean((sigma - abs(returns))^2)),
    vol_error = mean(sigma - abs(returns)),
    stability = mean(abs(diff(log(sigma)))))
}

#' Shape of a volatility path: level, spread, range and stability.
#'
#' @param sigma A volatility path; non-finite entries are dropped.
#' @return A named numeric vector: mean, sd, min, max and stability, the mean
#'   absolute change of ln sigma from one session to the next.
vol_path_shape <- function(sigma) {
  sigma <- sigma[is.finite(sigma)]
  c(mean = mean(sigma), sd = stats::sd(sigma), min = min(sigma), max = max(sigma),
    stability = mean(abs(diff(log(sigma)))))
}

#' Weight that each variance model puts on the squared return k sessions back.
#'
#' Unrolling the recursions: MA(m) gives 1/m to each of the last m returns and
#' nothing after; EWMA gives (1 - lambda) lambda^(k - 1), a geometric decay
#' that never reaches zero.
#'
#' @param lags Lags k = 1, 2, ...
#' @param m Moving-average window.
#' @param lambda EWMA decay factor.
#' @return A data frame: lag, ma and ewma weights.
variance_lag_weights <- function(lags, m, lambda) {
  stopifnot(all(lags >= 1))
  data.frame(lag = lags, ma = ifelse(lags <= m, 1 / m, 0),
             ewma = (1 - lambda) * lambda^(lags - 1))
}

#' Monthly realised volatility against each model's mean forecast.
#'
#' @param dates Session dates.
#' @param returns Log returns aligned with dates.
#' @param sigmas A named list of volatility paths aligned with dates.
#' @return One row per calendar month: month, sessions, realised (the root of
#'   the mean squared return) and one column per model (the root of its mean
#'   forecast variance).
vol_by_month <- function(dates, returns, sigmas) {
  month <- format(dates, "%Y-%m")
  out <- data.frame(month = sort(unique(month)), stringsAsFactors = FALSE)
  out$sessions <- as.numeric(table(month)[out$month])
  out$realised <- sqrt(as.numeric(tapply(returns^2, month, mean)[out$month]))
  for (name in names(sigmas)) {
    out[[name]] <- sqrt(as.numeric(tapply(sigmas[[name]]^2, month, mean)[out$month]))
  }
  out
}

#' Does each model keep up in the turbulent months?
#'
#' Splits the months of vol_by_month() into the most volatile share, by
#' realised volatility, and the rest, and divides each model's mean forecast
#' variance by the realised variance in each group: one is a perfect level,
#' below one underestimation, above one overestimation.
#'
#' @param by_month Output of vol_by_month().
#' @param models Model columns to score.
#' @param share Share of months classed as high volatility.
#' @return A data frame: model, high and other ratios, and the number of high
#'   months.
capture_ratios <- function(by_month, models, share = 0.25) {
  high <- by_month$realised >= stats::quantile(by_month$realised, 1 - share)
  ratio <- function(column, rows) {
    sum(by_month[[column]][rows]^2 * by_month$sessions[rows]) /
      sum(by_month$realised[rows]^2 * by_month$sessions[rows])
  }
  data.frame(model = models,
             high = vapply(models, ratio, numeric(1), rows = high),
             other = vapply(models, ratio, numeric(1), rows = !high),
             n_high = sum(high), stringsAsFactors = FALSE, row.names = NULL)
}

#' How a volatility path responds to one shock.
#'
#' @param sigma A volatility path.
#' @param at Index of the shock session; sigma[at] is the forecast made the
#'   day before it, the last one that has not seen the shock.
#' @param horizon Sessions to follow after the shock.
#' @return A list: path, sigma[at + 0:horizon] / sigma[at]; jump, the ratio
#'   the session after the shock; peak, the largest ratio; and half_back, the
#'   sessions after the shock until the estimate has given back half of its
#'   rise from the pre-shock level to the peak (NA if not within horizon).
shock_response <- function(sigma, at, horizon = 20) {
  stopifnot(at >= 1, at + horizon <= length(sigma))
  path <- sigma[at + 0:horizon] / sigma[at]
  peak_k <- which.max(path)
  threshold <- 1 + (path[peak_k] - 1) / 2
  after <- which(path[seq.int(peak_k, length(path))] <= threshold)
  list(path = path, jump = path[2], peak = path[peak_k],
       half_back = if (length(after) == 0) NA_real_ else peak_k + after[1] - 2)
}
