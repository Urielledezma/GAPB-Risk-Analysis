test_that("gbm_params separates the log drift from the arithmetic drift", {
  set.seed(19)
  returns <- rnorm(5000, mean = 0.0005, sd = 0.02)

  gbm <- gbm_params(returns, trading_days = 252)

  expect_equal(gbm$nu_daily, mean(returns))
  expect_equal(gbm$sigma_daily, sd(returns))
  # mu = nu + sigma^2 / 2 is the whole point of the parameterisation.
  expect_equal(gbm$mu_daily, gbm$nu_daily + gbm$sigma_daily^2 / 2)
  expect_gt(gbm$mu_daily, gbm$nu_daily)
})

test_that("the projection is exact at horizon zero", {
  projection <- gbm_projection(s0 = 100, nu = 0.0005, sigma = 0.02, horizons = 0)

  expect_equal(projection$expected, 100)
  expect_equal(projection$median, 100)
  expect_equal(projection$lower, 100)
  expect_equal(projection$upper, 100)
})

test_that("the expected price exceeds the median, and by more as the horizon grows", {
  projection <- gbm_projection(
    s0 = 100, nu = 0.0005, sigma = 0.02, horizons = c(10, 250)
  )

  expect_true(all(projection$expected > projection$median))
  gap <- projection$expected / projection$median
  expect_gt(gap[2], gap[1])
})

test_that("the confidence interval is asymmetric around the median and widens with sqrt(t)", {
  projection <- gbm_projection(
    s0 = 100, nu = 0, sigma = 0.02, horizons = c(10, 40)
  )

  # Lognormal: the upper arm is further from the median than the lower arm.
  upper_gap <- projection$upper - projection$median
  lower_gap <- projection$median - projection$lower
  expect_true(all(upper_gap > lower_gap))

  # Quadrupling the horizon doubles the log-width.
  log_width <- log(projection$upper) - log(projection$lower)
  expect_equal(log_width[2] / log_width[1], 2, tolerance = 1e-8)
})

test_that("the interval bounds carry the intended coverage", {
  projection <- gbm_projection(
    s0 = 100, nu = 0, sigma = 0.02, horizons = 25, confidence = 0.95
  )

  # ln(upper/S0) should sit at 1.96 standard deviations.
  z <- log(projection$upper / 100) / (0.02 * sqrt(25))
  expect_equal(z, qnorm(0.975), tolerance = 1e-8)
})

test_that("simulated paths reproduce the closed-form projection", {
  nu <- 0.0004
  sigma <- 0.015
  s0 <- 250

  paths <- gbm_paths(s0, nu, sigma, n_steps = 20, n_paths = 40000, seed = 99)
  simulated <- gbm_terminal_summary(paths)
  analytic <- gbm_projection(s0, nu, sigma, horizons = 20)

  expect_equal(nrow(paths), 21)
  expect_equal(ncol(paths), 40000)
  expect_true(all(paths[1, ] == s0))

  # Monte Carlo error at 40k paths: well inside 1%.
  expect_equal(simulated$expected, analytic$expected, tolerance = 0.01)
  expect_equal(simulated$median, analytic$median, tolerance = 0.01)
  expect_equal(simulated$lower, analytic$lower, tolerance = 0.02)
  expect_equal(simulated$upper, analytic$upper, tolerance = 0.02)
})

test_that("paths are reproducible for a given seed", {
  first <- gbm_paths(100, 0, 0.02, n_steps = 5, n_paths = 100, seed = 1)
  second <- gbm_paths(100, 0, 0.02, n_steps = 5, n_paths = 100, seed = 1)

  expect_identical(first, second)
})
