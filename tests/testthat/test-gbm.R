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

test_that("the projection carries the class summary: variance, mode and their ordering", {
  projection <- gbm_projection(s0 = 100, nu = 0.05, sigma = 0.3, horizons = c(0.25, 1))
  mu <- 0.05 + 0.3^2 / 2

  expect_equal(projection$variance, 100^2 * exp(2 * mu * c(0.25, 1)) * (exp(0.3^2 * c(0.25, 1)) - 1))
  expect_equal(projection$sd, sqrt(projection$variance))
  expect_equal(projection$mode, 100 * exp((mu - 1.5 * 0.3^2) * c(0.25, 1)))
  # Right skew: mean above median above mode, at every horizon.
  expect_true(all(projection$expected > projection$median))
  expect_true(all(projection$median > projection$mode))
  expect_equal(projection$interval_width, projection$upper - projection$lower)
  expect_equal(projection$relative_width, projection$interval_width / 100)
})

test_that("daily and annual parameters describe the same model in different units", {
  nu_d <- 0.0007
  sigma_d <- 0.02
  daily <- gbm_projection(250, nu_d, sigma_d, horizons = 63)
  annual <- gbm_projection(250, nu_d * 252, sigma_d * sqrt(252), horizons = 63 / 252)

  expect_equal(daily$expected, annual$expected, tolerance = 1e-12)
  expect_equal(daily$lower, annual$lower, tolerance = 1e-12)
  expect_equal(daily$upper, annual$upper, tolerance = 1e-12)
})

test_that("the local projection is exact at horizon zero and symmetric around its mean", {
  zero <- gbm_local_projection(s0 = 100, mu = 0.001, sigma = 0.02, horizons = 0)
  expect_equal(c(zero$expected, zero$lower, zero$upper), c(100, 100, 100))

  local <- gbm_local_projection(s0 = 100, mu = 0.001, sigma = 0.02, horizons = c(10, 40))
  expect_equal(local$expected, 100 * (1 + 0.001 * c(10, 40)))
  expect_equal(local$upper - local$expected, local$expected - local$lower)
  # The upper bound sits exactly 1.96 standard deviations above the mean.
  expect_equal((local$upper - local$expected) / (0.02 * 100 * sqrt(c(10, 40))),
               rep(qnorm(0.975), 2))
})

test_that("draws reproduce their closed forms, and the check reports the coverage", {
  set.seed(456)
  z <- rnorm(100000)
  s0 <- 300

  exact_theory <- gbm_projection(s0, nu = 0.1, sigma = 0.3, horizons = 1)
  exact <- gbm_mc_check(gbm_exact_draws(s0, 0.1, 0.3, 1, z), exact_theory)
  expect_lt(abs(exact$mean_gap), 0.005)
  expect_lt(abs(exact$var_gap), 0.03)
  expect_lt(abs(exact$coverage - 0.95), 4 * exact$coverage_mc_error)

  local_theory <- gbm_local_projection(s0, mu = 0.001, sigma = 0.02, horizons = 20)
  local <- gbm_mc_check(gbm_local_draws(s0, 0.001, 0.02, 20, z), local_theory)
  expect_lt(abs(local$mean_gap), 0.002)
  expect_lt(abs(local$coverage - 0.95), 4 * local$coverage_mc_error)
})

test_that("an Euler step is the class recursion, and the drift compounds", {
  z <- matrix(c(0.5, -1, 2, 0), nrow = 2)
  paths <- gbm_euler_paths(100, mu = 0.01, sigma = 0.02, z = z)

  expect_equal(dim(paths), c(3, 2))
  expect_equal(paths[1, ], c(100, 100))
  expect_equal(paths[2, 1], 100 + 0.01 * 100 + 0.02 * 100 * 0.5)
  expect_equal(paths[3, 1], paths[2, 1] * (1 + 0.01 + 0.02 * -1))

  # With no shocks the path is pure compounding, (1 + mu)^n.
  flat <- gbm_euler_paths(100, mu = 0.01, sigma = 0.02, z = matrix(0, nrow = 5, ncol = 1))
  expect_equal(flat[6, 1], 100 * 1.01^5)
})

test_that("the loss probability matches the simulated frequency", {
  set.seed(8)
  z <- rnorm(200000)
  simulated <- mean(gbm_exact_draws(100, nu = 0.08, sigma = 0.3, horizon = 0.5, z) < 100)

  expect_equal(gbm_loss_probability(0.08, 0.3, 0.5), simulated, tolerance = 0.005)
  # A positive log drift makes a loss less likely the longer the horizon.
  probabilities <- gbm_loss_probability(0.08, 0.3, c(0.25, 1, 4))
  expect_true(all(diff(probabilities) < 0))
})

test_that("the standardised outcome is zero at the median and 1.96 at the upper bound", {
  projection <- gbm_projection(100, nu = 0.05, sigma = 0.3, horizons = 0.5)

  expect_equal(gbm_standardised_outcome(100, projection$median, 0.05, 0.3, 0.5), 0)
  expect_equal(gbm_standardised_outcome(100, projection$upper, 0.05, 0.3, 0.5), qnorm(0.975))
})
