test_that("log returns invert to the original price path", {
  prices <- data.frame(
    date = as.Date("2020-01-01") + 0:9,
    ticker = "TEST",
    adjusted = c(100, 102, 101, 105, 103, 108, 107, 110, 109, 112)
  )

  returns <- compute_log_returns(prices)

  expect_equal(nrow(returns), nrow(prices) - 1)
  # Reconstructing the path from the returns must reproduce it exactly.
  expect_equal(
    prices$adjusted[1] * exp(cumsum(returns$log_return)),
    prices$adjusted[-1]
  )
})

test_that("the first observation of each ticker is dropped, not carried", {
  prices <- data.frame(
    date = rep(as.Date("2020-01-01") + 0:4, 2),
    ticker = rep(c("A", "B"), each = 5),
    adjusted = c(10, 11, 12, 13, 14, 100, 99, 98, 97, 96)
  )

  returns <- compute_log_returns(prices)

  expect_equal(nrow(returns), 8)
  expect_equal(as.integer(table(returns$ticker)), c(4L, 4L))
  # A cross-ticker return would show up as a huge jump on B's first row.
  expect_lt(max(abs(returns$log_return)), 0.15)
})

test_that("annualisation follows the additive and square-root identities", {
  expect_equal(annualise_mean(0.0004, 252), 0.0004 * 252)
  expect_equal(annualise_vol(0.02, 252), 0.02 * sqrt(252))

  moments <- return_moments(rnorm(1000, 0.0004, 0.02), trading_days = 252)
  expect_equal(moments$mean_annual, moments$mean_daily * 252)
  expect_equal(moments$vol_annual, moments$vol_daily * sqrt(252))
})

test_that("maximum drawdown finds the true peak and trough", {
  path <- c(100, 120, 90, 60, 80, 130, 125)
  dates <- as.Date("2020-01-01") + seq_along(path) - 1

  result <- max_drawdown(path, dates)

  # Peak 120 to trough 60 is a 50% decline.
  expect_equal(result$max_drawdown, -0.5)
  expect_equal(result$peak_date, dates[2])
  expect_equal(result$trough_date, dates[4])
  expect_equal(result$recovery_date, dates[6])
})

test_that("drawdown is never positive and is zero at every new high", {
  path <- c(10, 12, 14, 11, 9, 15)
  drawdown <- drawdown_series(path)

  expect_true(all(drawdown <= 0))
  expect_equal(drawdown[c(1, 2, 3, 6)], rep(0, 4))
})

test_that("returns_matrix keeps only dates common to every ticker", {
  returns <- data.frame(
    date = as.Date(c(
      "2020-01-01", "2020-01-02", "2020-01-03",
      "2020-01-02", "2020-01-03"
    )),
    ticker = c("A", "A", "A", "B", "B"),
    log_return = c(0.01, 0.02, 0.03, -0.01, -0.02)
  )

  matrix_returns <- returns_matrix(returns)

  expect_equal(nrow(matrix_returns), 2)
  expect_equal(sort(colnames(matrix_returns)), c("A", "B"))
  expect_false("2020-01-01" %in% rownames(matrix_returns))
})
