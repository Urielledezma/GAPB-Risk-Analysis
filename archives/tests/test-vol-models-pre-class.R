# Archived from tests/testthat/test-vol-models.R on 2026-11-05. Not sourced, linted or tested.
#
# Tests of archives/R/vol_models_pre_class.R; they need that file sourced.

test_that("EWMA follows the recursion exactly", {
  set.seed(23)
  returns <- rnorm(100, sd = 0.02)
  lambda <- 0.94
  burn_in <- 20

  sigma <- ewma_volatility(returns, lambda = lambda, burn_in = burn_in)

  expect_true(all(is.na(sigma[1:(burn_in - 1)])))
  expect_equal(sigma[burn_in]^2, var(returns[1:burn_in]))

  # Recompute a few steps by hand.
  manual <- sigma[burn_in]^2
  for (t in (burn_in + 1):(burn_in + 5)) {
    manual <- lambda * manual + (1 - lambda) * returns[t - 1]^2
    expect_equal(sigma[t]^2, manual)
  }
})

test_that("a lower lambda reacts faster to a volatility shock", {
  returns <- c(rep(0.002, 60), rep(0.06, 20))

  fast <- ewma_volatility(returns, lambda = 0.85)
  slow <- ewma_volatility(returns, lambda = 0.99)

  # Five days after the regime change the fast model has moved further.
  expect_gt(fast[66], slow[66])
})

test_that("ewma_fit_lambda returns a lambda inside the grid under both losses", {
  set.seed(29)
  returns <- c(rnorm(300, sd = 0.01), rnorm(300, sd = 0.04))
  grid <- seq(0.80, 0.99, by = 0.01)

  rmse_fit <- ewma_fit_lambda(returns, grid = grid, loss = "rmse")
  loglik_fit <- ewma_fit_lambda(returns, grid = grid, loss = "loglik")

  expect_true(rmse_fit$lambda %in% grid)
  expect_true(loglik_fit$lambda %in% grid)
  expect_equal(rmse_fit$alpha, 1 - rmse_fit$lambda)
  expect_equal(nrow(rmse_fit$surface), length(grid))
  expect_false(any(is.na(loglik_fit$surface$score)))
})

test_that("score_volatility rewards the forecast closer to the realised series", {
  realised <- abs(rnorm(500, sd = 0.02))
  good <- realised + rnorm(500, sd = 0.001)
  bad <- realised + rnorm(500, sd = 0.02)

  good_score <- score_volatility(good, realised, "good")
  bad_score <- score_volatility(bad, realised, "bad")

  expect_lt(good_score$rmse, bad_score$rmse)
  expect_lt(good_score$mae, bad_score$mae)
  expect_equal(good_score$model, "good")
})

test_that("score_volatility bias is signed", {
  realised <- rep(0.02, 100)

  expect_gt(score_volatility(rep(0.03, 100), realised)$bias, 0)
  expect_lt(score_volatility(rep(0.01, 100), realised)$bias, 0)
})
