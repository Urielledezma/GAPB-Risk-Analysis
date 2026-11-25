test_that("coverage counts strictly lower outcomes with an available forecast", {
  pnl <- c(-100, -30, -20, rep(0, 98))
  threshold <- c(NA_real_, rep(-20, 100))
  result <- backtest_var(pnl, threshold, confidence = 0.99)

  expect_equal(result$n, 100)
  expect_equal(result$expected_breaches, 1)
  expect_equal(result$observed_breaches, 1)
  expect_equal(result$p_value, 1)
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

test_that("Kupiec coverage uses the limiting likelihood at zero and all breaches", {
  confidence <- 0.95
  threshold <- rep(-20, 100)
  zero <- backtest_var(rep(0, 100), threshold, confidence, alpha = 0.05)
  all <- backtest_var(rep(-30, 100), threshold, confidence, alpha = 0.05)

  for (result in list(zero, all)) {
    n <- result$n
    expected_lr <- if (result$observed_breaches == 0) {
      -2 * n * log(confidence)
    } else {
      -2 * n * log1p(-confidence)
    }
    expect_equal(result$lr_statistic, expected_lr)
    expect_equal(result$p_value, stats::pchisq(expected_lr, 1, lower.tail = FALSE))
    expect_true(result$reject_model)
  }
  expect_equal(zero$observed_breaches, 0)
  expect_equal(all$observed_breaches, 100)
  expect_false(backtest_var(0, -20, confidence, alpha = 0.05)$reject_model)
})

test_that("coverage requires at least one usable forecast", {
  result <- backtest_var(c(-12, NA_real_), c(NA_real_, -10), confidence = 0.99)
  expect_equal(result$n, 0)
  expect_true(is.na(result$lr_statistic))
  expect_true(is.na(result$p_value))
  expect_false(result$reject_model)
})
