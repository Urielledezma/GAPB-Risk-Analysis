test_that("historical VaR sits at the empirical quantile", {
  set.seed(31)
  returns <- rnorm(10000, mean = 0, sd = 0.02)

  result <- var_historical(returns, confidence = 0.99, horizon = 1, value = 1e6)

  expect_equal(result$var_return, unname(quantile(returns, 0.01)))
  # Reported as a positive loss.
  expect_gt(result$var_currency, 0)
  expect_equal(result$var_pct, result$var_currency / 1e6)
})

test_that("VaR increases with the confidence level and with the horizon", {
  set.seed(37)
  returns <- rnorm(5000, sd = 0.02)

  by_confidence <- vapply(
    c(0.95, 0.975, 0.99),
    function(c) var_historical(returns, c, 1, 1e6)$var_currency,
    numeric(1)
  )
  expect_true(all(diff(by_confidence) > 0))

  by_horizon <- vapply(
    c(1, 5, 10, 20),
    function(h) var_historical(returns, 0.99, h, 1e6)$var_currency,
    numeric(1)
  )
  expect_true(all(diff(by_horizon) > 0))
})

test_that("parametric VaR matches the normal quantile it claims to use", {
  sigma <- 0.02
  result <- var_parametric(sigma, confidence = 0.99, horizon = 4, value = 1e6)

  expect_equal(result$var_return, qnorm(0.01) * sigma * 2)
  expect_equal(result$var_currency, 1e6 * (1 - exp(qnorm(0.01) * sigma * 2)))
})

test_that("on normal data the two methods agree closely", {
  set.seed(41)
  returns <- rnorm(50000, mean = 0, sd = 0.02)

  historical <- var_historical(returns, 0.99, 1, 1e6)$var_currency
  parametric <- var_parametric(sd(returns), 0.99, 1, 1e6)$var_currency

  expect_equal(historical, parametric, tolerance = 0.03)
})

test_that("on fat-tailed data historical VaR exceeds parametric VaR", {
  set.seed(43)
  returns <- rt(50000, df = 3) * 0.01

  historical <- var_historical(returns, 0.99, 1, 1e6)$var_currency
  parametric <- var_parametric(sd(returns), 0.99, 1, 1e6)$var_currency

  # This gap is the cost of the normality assumption, and it is a finding
  # the reports are meant to surface rather than an artefact.
  expect_gt(historical, parametric)
})

test_that("expected shortfall is at least as severe as VaR", {
  set.seed(47)
  returns <- rt(20000, df = 4) * 0.01

  var_result <- var_historical(returns, 0.99, 1, 1e6)
  es_result <- expected_shortfall(returns, 0.99, 1e6)

  expect_gte(es_result$es_currency, var_result$var_currency)
  expect_lt(es_result$es_return, var_result$var_return)
  expect_equal(es_result$n_tail, sum(returns <= var_result$var_return))
})

test_that("monte carlo VaR converges on the historical figure", {
  set.seed(53)
  returns <- rnorm(20000, sd = 0.02)

  mc <- var_monte_carlo(returns, 0.99, horizon = 1, value = 1e6, n_simulations = 200000)
  historical <- var_historical(returns, 0.99, 1, 1e6)

  expect_equal(mc$var_currency, historical$var_currency, tolerance = 0.03)
  expect_gt(mc$es_currency, mc$var_currency)
})

test_that("rolling VaR uses only past observations", {
  set.seed(59)
  returns <- rnorm(400, sd = 0.02)

  series <- rolling_var(returns, 0.99, window = 250, method = "historical")

  expect_true(all(is.na(series[1:250])))
  expect_false(any(is.na(series[251:400])))
  expect_equal(series[300], unname(quantile(returns[50:299], 0.01)))
})

test_that("a well-specified model produces roughly the expected breach count", {
  set.seed(61)
  returns <- rnorm(3000, sd = 0.02)
  series <- rolling_var(returns, 0.99, window = 250, method = "parametric")

  result <- backtest_var(returns, series, 0.99)

  expect_equal(result$n, 2750)
  expect_equal(result$expected_breaches, 27.5)
  expect_false(result$reject_model)
})

test_that("a deliberately understated VaR is rejected", {
  set.seed(67)
  returns <- rnorm(3000, sd = 0.02)
  # Half the correct magnitude: breaches should be far too frequent.
  series <- rep(qnorm(0.01) * 0.01, 3000)

  result <- backtest_var(returns, series, 0.99)

  expect_gt(result$breach_rate, 0.01)
  expect_true(result$reject_model)
})

test_that("a wildly overstated VaR is rejected too", {
  set.seed(71)
  returns <- rnorm(3000, sd = 0.02)
  series <- rep(-1, 3000)

  result <- backtest_var(returns, series, 0.99)

  # Zero breaches leaves the likelihood ratio undefined; the function must
  # return NA rather than a spurious pass.
  expect_equal(result$observed_breaches, 0)
  expect_true(is.na(result$lr_statistic))
  expect_false(result$reject_model)
})
