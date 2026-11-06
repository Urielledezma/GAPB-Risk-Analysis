test_that("the half-life of a geometric decay is ln(0.5) / ln(decay)", {
  expect_equal(decay_half_life(0.5), 1)
  expect_equal(decay_half_life(0.94), log(0.5) / log(0.94))
  expect_error(decay_half_life(1))
})

test_that("the class EWMA is seeded with r_1^2 and follows the recursion", {
  returns <- c(0.02, -0.01, 0.03, 0.00, -0.02)
  lambda <- 0.9

  sigma2 <- ewma_class_variance(returns, lambda)

  expect_equal(sigma2[1], returns[1]^2)
  expect_equal(sigma2[2], 0.1 * returns[1]^2 + 0.9 * sigma2[1])
  expect_equal(sigma2[5], 0.1 * returns[4]^2 + 0.9 * sigma2[4])
})

test_that("the class EWMA log-likelihood is the sum of -ln s2 - r2 / s2", {
  returns <- c(0.02, -0.01, 0.03, 0.01, -0.02)
  sigma2 <- ewma_class_variance(returns, 0.8)

  expect_equal(ewma_class_loglik(returns, 0.8), sum(-log(sigma2) - returns^2 / sigma2))
  expect_error(ewma_class_loglik(c(0, returns), 0.8), "first return is zero")
})

test_that("the ML lambda beats every lambda on the profile grid", {
  set.seed(11)
  returns <- rnorm(400, sd = rep(c(0.01, 0.03), each = 100, times = 2))

  fit <- ewma_class_fit(returns, interval = c(0.001, 0.9999))
  profile <- ewma_loglik_profile(returns, grid = seq(0.80, 0.995, by = 0.005))

  expect_true(fit$lambda > 0.001 && fit$lambda < 0.9999)
  expect_gte(fit$loglik, max(profile$loglik) - 1e-6)
  expect_equal(nrow(profile), length(seq(0.80, 0.995, by = 0.005)))
})

test_that("an EWMA path estimates lambda on the first n_fit returns only", {
  set.seed(12)
  returns <- rnorm(300, sd = 0.02)
  altered <- returns
  altered[251:300] <- altered[251:300] * 5

  path <- ewma_class_path(returns, n_fit = 250)
  path_altered <- ewma_class_path(altered, n_fit = 250)

  expect_equal(path$lambda, path_altered$lambda)
  expect_length(path$sigma, 300)
  expect_equal(path$sigma^2, ewma_class_variance(returns, path$lambda))
  expect_equal(path$next_sigma^2,
               (1 - path$lambda) * returns[300]^2 + path$lambda * path$sigma[300]^2)
})

test_that("rho is E|r - rbar| / sigma, close to sqrt(2 / pi) for normal returns", {
  set.seed(13)
  returns <- rnorm(20000, mean = 0.001, sd = 0.02)

  scale <- ddewma_scale(returns)

  expect_equal(scale$mean, mean(returns))
  expect_equal(scale$rho, sqrt(2 / pi), tolerance = 0.01)
  # Heavier tails lower rho.
  heavy <- ddewma_scale(0.02 * rt(20000, df = 3))
  expect_lt(heavy$rho, scale$rho)
})

test_that("the DDEWMA weighs the newest |r - rbar| / rho by alpha", {
  returns <- c(0.02, -0.01, 0.03, 0.01)
  scale <- list(mean = 0.005, rho = 0.8)
  z <- abs(returns - 0.005) / 0.8

  sigma <- ddewma_volatility(returns, alpha = 0.2, scale = scale)

  expect_equal(sigma[1], z[1])
  expect_equal(sigma[2], 0.2 * z[1] + 0.8 * sigma[1])
  expect_equal(sigma[4], 0.2 * z[3] + 0.8 * sigma[3])
  expect_equal(ddewma_sse(returns, 0.2, scale), sum((z - sigma)^2))
})

test_that("the least-squares alpha beats a grid and the path freezes its parameters", {
  set.seed(14)
  returns <- rnorm(300, sd = rep(c(0.01, 0.025), each = 75, times = 2))

  fit <- ddewma_fit(returns, interval = c(0.001, 0.9999))
  scale <- ddewma_scale(returns)
  grid_sse <- vapply(seq(0.01, 0.99, by = 0.01), function(a) ddewma_sse(returns, a, scale),
                     numeric(1))
  expect_lte(fit$sse, min(grid_sse) + 1e-9)

  altered <- returns
  altered[201:300] <- 0.1
  frozen <- ddewma_path(returns, n_fit = 200)
  frozen_altered <- ddewma_path(altered, n_fit = 200)
  expect_equal(frozen$alpha, frozen_altered$alpha)
  expect_equal(frozen$rho, frozen_altered$rho)
  expect_length(frozen$sigma, 300)
  z_last <- abs(returns[300] - frozen$mean) / frozen$rho
  expect_equal(frozen$next_sigma, frozen$alpha * z_last + (1 - frozen$alpha) * frozen$sigma[300])
})

test_that("the ARCH-LM statistic is T * R^2 and separates ARCH from white noise", {
  set.seed(15)
  noise <- rnorm(2000)
  test_noise <- arch_lm_test(noise, lags = 5)

  lagged <- stats::embed(noise^2, 6)
  aux <- stats::lm(lagged[, 1] ~ lagged[, -1])
  expect_equal(test_noise$statistic, nrow(lagged) * summary(aux)$r.squared)
  expect_equal(test_noise$df, 5)
  expect_gt(test_noise$p_value, 0.01)

  # An ARCH(1) process with alpha = 0.5.
  arch <- numeric(2000)
  for (t in 2:2000) arch[t] <- rnorm(1, sd = sqrt(0.2 + 0.5 * arch[t - 1]^2))
  expect_lt(arch_lm_test(arch, lags = 5)$p_value, 1e-6)
})

test_that("the six class candidates are labelled by their orders", {
  candidates <- garch_class_candidates(list(c(1, 0), c(2, 0), c(1, 1), c(1, 2), c(2, 1), c(2, 2)))

  expect_equal(candidates$model,
               c("ARCH(1)", "ARCH(2)", "GARCH(1,1)", "GARCH(1,2)", "GARCH(2,1)", "GARCH(2,2)"))
  expect_equal(candidates$q, c(0, 0, 1, 2, 1, 2))
  expect_equal(nrow(garch_class_candidates()), 6)
})

test_that("the class selection keeps significant, stationary models and takes the lowest BIC", {
  summary <- data.frame(
    model = c("ARCH(1)", "GARCH(1,1)", "GARCH(1,2)", "GARCH(2,2)"),
    converged = c(TRUE, TRUE, TRUE, FALSE),
    all_significant = c(TRUE, TRUE, FALSE, TRUE),
    persistence = c(0.3, 0.9, 0.9, 0.95),
    bic = c(-100, -110, -130, -140),
    stringsAsFactors = FALSE
  )
  expect_equal(garch_class_select(summary), "GARCH(1,1)")

  summary$persistence[2] <- 1.01
  expect_equal(garch_class_select(summary), "ARCH(1)")

  summary$all_significant <- FALSE
  expect_error(garch_class_select(summary), "no candidate")
})

test_that("GARCH starting pairs are a full grid inside the stationary region", {
  starts <- garch_class_starts(c(0.05, 0.15), c(0.6, 0.8))

  expect_equal(nrow(starts), 4)
  expect_true(all(starts$alpha + starts$beta < 1))
  expect_error(garch_class_starts(0.3, 0.8))
})

test_that("searching from several starts never ends below the default search", {
  skip_if_not_installed("rugarch")
  set.seed(17)
  returns <- rnorm(500, sd = 0.015)
  returns[c(120, 300)] <- c(-0.15, 0.12)

  default_only <- garch_class_fit(returns, 1, 1, c(0, 0), starts = NULL)
  searched <- garch_class_fit(returns, 1, 1, c(0, 0),
                              starts = garch_class_starts(c(0.05, 0.15), c(0.6, 0.8)))

  expect_gte(rugarch::likelihood(searched), rugarch::likelihood(default_only) - 1e-8)
})

test_that("a failed GARCH fit gives an empty summary row", {
  row <- garch_class_summary("GARCH(1,1)", NULL, n_fit = 100)

  expect_false(row$converged)
  expect_false(row$all_significant)
  expect_true(is.na(row$bic))
})

test_that("a class GARCH path is fitted on n_fit returns and rolled forward", {
  skip_if_not_installed("rugarch")
  set.seed(16)
  n <- 700
  returns <- numeric(n)
  sigma2 <- numeric(n)
  sigma2[1] <- 1e-4
  for (t in 2:n) {
    sigma2[t] <- 1e-5 + 0.1 * returns[t - 1]^2 + 0.8 * sigma2[t - 1]
    returns[t] <- rnorm(1, sd = sqrt(sigma2[t]))
  }

  path <- garch_class_path(returns, p = 1, q = 1, arma_order = c(0, 0), n_fit = 600)
  row <- garch_class_summary("GARCH(1,1)", path$fit, n_fit = 600)

  expect_length(path$sigma, n)
  expect_true(all(path$sigma > 0))
  expect_true(row$converged)
  expect_equal(row$aic, -2 * row$loglik + 2 * row$k)
  expect_equal(row$bic, -2 * row$loglik + log(600) * row$k)

  # The rolled part is the GARCH recursion with the fitted coefficients.
  cf <- rugarch::coef(path$fit)
  eps <- returns - cf[["mu"]]
  expect_equal(path$sigma[601]^2,
               cf[["omega"]] + cf[["alpha1"]] * eps[600]^2 + cf[["beta1"]] * path$sigma[600]^2,
               tolerance = 1e-8)
  expect_equal(path$next_sigma^2,
               cf[["omega"]] + cf[["alpha1"]] * eps[n]^2 + cf[["beta1"]] * path$sigma[n]^2,
               tolerance = 1e-8)

  long_run <- garch_long_run(path$fit)
  expect_equal(long_run$long_run_variance, cf[["omega"]] / (1 - cf[["alpha1"]] - cf[["beta1"]]))
})

test_that("the Gaussian log-likelihood is the normal density and rescales the class one", {
  set.seed(19)
  returns <- rnorm(200, sd = 0.02)
  sigma <- sqrt(ewma_class_variance(returns, 0.9))

  expect_equal(gaussian_loglik(returns, sigma), sum(dnorm(returns, sd = sigma, log = TRUE)))
  expect_equal(gaussian_loglik(returns, sigma),
               ewma_class_loglik(returns, 0.9) / 2 - 200 / 2 * log(2 * pi))
  expect_error(gaussian_loglik(returns, c(sigma[-1], 0)))
})

test_that("model information scores every model on the same sessions", {
  set.seed(20)
  returns <- rnorm(100, sd = 0.02)
  sigmas <- list(flat = rep(0.02, 100), wrong = rep(0.05, 100))
  residuals <- list(flat = returns, wrong = returns)

  info <- model_information(residuals, sigmas, k = c(flat = 1, wrong = 3), sessions = 11:100)

  expect_equal(info$n, c(90, 90))
  expect_equal(info$loglik[1], gaussian_loglik(returns[11:100], rep(0.02, 90)))
  expect_equal(info$aic, -2 * info$loglik + 2 * c(1, 3))
  expect_equal(info$bic, -2 * info$loglik + log(90) * c(1, 3))
  expect_lt(info$aic[1], info$aic[2])
})

test_that("the class VaR is |Z| V0 sigma", {
  expect_equal(class_var(0.02, 100000, 0.95), qnorm(0.95) * 100000 * 0.02)
  expect_gt(class_var(0.02, 100000, 0.99), class_var(0.02, 100000, 0.95))
  expect_error(class_var(0.02, 100000, 0.4))
})

test_that("the ARMA benchmark row uses the class AIC and BIC and is never selectable", {
  skip_if_not_installed("forecast")
  set.seed(18)
  returns <- rnorm(400, sd = 0.02)

  arma <- arma_mean_order(returns)
  row <- arma_benchmark_summary(arma$fit, n_fit = 400, name = "Varianza constante")
  ll <- as.numeric(stats::logLik(arma$fit))
  k <- length(stats::coef(arma$fit)) + 1

  expect_length(arma$order, 2)
  expect_equal(row$aic, -2 * ll + 2 * k)
  expect_equal(row$bic, -2 * ll + log(400) * k)
  expect_equal(row$long_run_variance, arma$fit$sigma2)
  expect_false(row$all_significant)
})

test_that("the scorecard measures loss, bias and stability on the right scales", {
  returns <- c(0.01, -0.02, 0.015, -0.005, 0.03)
  sigma <- c(0.012, 0.015, 0.02, 0.018, 0.016)

  card <- forecast_scorecard(returns, sigma)

  expect_equal(card[c("rmse", "mae", "qlike", "relative")],
               ma_class_losses(returns^2, sigma^2))
  expect_equal(unname(card["qlike_log"]), mean(log(sigma^2) + returns^2 / sigma^2))
  expect_equal(unname(card["bias_ratio"]), mean(sigma^2) / mean(returns^2))
  expect_equal(unname(card["vol_error"]), mean(sigma - abs(returns)))
  expect_equal(unname(card["stability"]), mean(abs(diff(log(sigma)))))
  # A flat forecast is perfectly stable.
  expect_equal(unname(forecast_scorecard(returns, rep(0.02, 5))["stability"]), 0)
})

test_that("monthly volatility averages variances, not volatilities", {
  dates <- as.Date(c("2026-01-05", "2026-01-06", "2026-02-02", "2026-02-03"))
  returns <- c(0.01, 0.03, -0.02, 0.02)
  sigmas <- list(model = c(0.01, 0.02, 0.02, 0.02))

  out <- vol_by_month(dates, returns, sigmas)

  expect_equal(out$month, c("2026-01", "2026-02"))
  expect_equal(out$sessions, c(2, 2))
  expect_equal(out$realised[1], sqrt(mean(c(0.01, 0.03)^2)))
  expect_equal(out$model[1], sqrt(mean(c(0.01, 0.02)^2)))
})

test_that("the path shape drops missing values and measures relative changes", {
  sigma <- c(NA, 0.01, 0.02, 0.01)
  shape <- vol_path_shape(sigma)

  expect_equal(unname(shape["mean"]), mean(c(0.01, 0.02, 0.01)))
  expect_equal(unname(shape["max"]), 0.02)
  expect_equal(unname(shape["stability"]), log(2))
})

test_that("lag weights follow the unrolled MA and EWMA recursions", {
  weights <- variance_lag_weights(1:50, m = 40, lambda = 0.9)

  expect_equal(weights$ma[1:40], rep(1 / 40, 40))
  expect_equal(weights$ma[41:50], rep(0, 10))
  expect_equal(weights$ewma[3], 0.1 * 0.9^2)
  expect_equal(sum(variance_lag_weights(1:5000, 40, 0.9)$ewma), 1)
})

test_that("capture ratios weigh each month by its sessions", {
  by_month <- data.frame(month = c("a", "b", "c", "d"), sessions = c(20, 20, 10, 20),
                         realised = c(0.01, 0.02, 0.03, 0.01),
                         model = c(0.01, 0.02, 0.015, 0.02))

  out <- capture_ratios(by_month, "model", share = 0.25)

  expect_equal(out$n_high, 1)
  expect_equal(out$high, 0.015^2 / 0.03^2)
  expect_equal(out$other, (20 * 0.01^2 + 20 * 0.02^2 + 20 * 0.02^2) /
                 (20 * 0.01^2 + 20 * 0.02^2 + 20 * 0.01^2))
})

test_that("the shock response reports the jump, the peak and the way back", {
  sigma <- c(1, 1, 1, 2, 3, 2.5, 2, 1.8, 1.5, 1.2, 1, 1)
  response <- shock_response(sigma, at = 3, horizon = 8)

  expect_equal(response$path, sigma[3:11])
  expect_equal(response$jump, 2)
  expect_equal(response$peak, 3)
  # Half way back from 3 to 1 is 2, reached four sessions after the shock.
  expect_equal(response$half_back, 4)
})
