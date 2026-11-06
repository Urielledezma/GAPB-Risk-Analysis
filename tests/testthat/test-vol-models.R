test_that("MA volatility equals the hand-computed window and uses only the past", {
  returns <- c(0.01, -0.02, 0.03, -0.01, 0.02, -0.03, 0.01)

  sigma <- ma_volatility(returns, m = 3)

  # First three entries have no full window behind them.
  expect_true(all(is.na(sigma[1:3])))
  # Entry 4 uses observations 1 to 3 only.
  expect_equal(sigma[4], sqrt(mean(returns[1:3]^2)))
  expect_equal(sigma[7], sqrt(mean(returns[4:6]^2)))
})

test_that("MA volatility never looks at the contemporaneous return", {
  returns <- c(rep(0.001, 10), 0.5)
  sigma <- ma_volatility(returns, m = 5)

  # The shock on day 11 must not appear in day 11's estimate.
  expect_equal(sigma[11], sqrt(mean(returns[6:10]^2)))
  expect_lt(sigma[11], 0.01)
})

test_that("trailing MA volatility includes the current session, as SMA does in class", {
  returns <- c(0.01, -0.02, 0.03, -0.01, 0.02)

  sigma <- ma_trailing_volatility(returns, m = 3)

  expect_true(all(is.na(sigma[1:2])))
  expect_equal(sigma[3], sqrt(mean(returns[1:3]^2)))
  expect_equal(sigma[5], sqrt(mean(returns[3:5]^2)))
  # The same average as the next-session forecast, one session earlier.
  expect_equal(sigma[3:4], ma_volatility(returns, 3)[4:5])
  expect_true(all(is.na(ma_trailing_volatility(returns[1:2], 3))))
})

test_that("the class losses match their definitions and drop zero-return days from QLIKE", {
  y <- c(4e-4, 1e-4, 0, 9e-4)
  f <- c(2e-4, 2e-4, 3e-4, 6e-4)

  losses <- ma_class_losses(y, f)

  expect_equal(unname(losses["rmse"]), sqrt(mean((y - f)^2)))
  expect_equal(unname(losses["mae"]), mean(abs(y - f)))
  # Day 3 has y = 0: its QLIKE term is NaN and is dropped, as na.rm does in class.
  keep <- y > 0
  expect_equal(unname(losses["qlike"]), mean(f[keep] / y[keep] - log(f[keep] / y[keep]) - 1))
  expect_equal(unname(losses["relative"]), mean(abs(1 - y / f)))
})

test_that("the window that wins most losses is chosen, and a tie goes to the lowest RMSE", {
  majority <- data.frame(m = c(5, 10, 20, 40),
                         rmse = c(4, 3, 2, 1), mae = c(4, 3, 2, 1),
                         qlike = c(1, 2, 3, 4), relative = c(4, 3, 2, 1))
  expect_equal(ma_select_window(majority), 40)

  # Two votes each: 20 wins RMSE and MAE, 40 wins QLIKE and the relative error.
  tie <- data.frame(m = c(5, 10, 20, 40),
                    rmse = c(4, 3, 1, 2), mae = c(4, 3, 1, 2),
                    qlike = c(4, 3, 2, 1), relative = c(4, 3, 2, 1))
  expect_equal(ma_select_window(tie), 20)
})

test_that("window losses follow the class 70/30 procedure", {
  set.seed(31)
  returns <- rnorm(200, sd = 0.02)
  windows <- c(5, 20)

  losses <- ma_window_losses(returns, windows = windows, train_share = 0.7)

  # By hand: the forecast for t is the mean of r^2 over t-m..t-1, scored on t > floor(0.7 n).
  test <- (floor(0.7 * 200) + 1):200
  for (k in seq_along(windows)) {
    m <- windows[k]
    f <- vapply(test, function(t) mean(returns[(t - m):(t - 1)]^2), numeric(1))
    expect_equal(losses$n_test[k], length(test))
    expect_equal(losses$rmse[k], sqrt(mean((returns[test]^2 - f)^2)))
    expect_equal(losses$mae[k], mean(abs(returns[test]^2 - f)))
  }
})

test_that("the next-session variance is the mean of the last m squared returns", {
  returns <- c(0.01, -0.02, 0.03, -0.04)
  expect_equal(ma_next_variance(returns, 2), mean(c(0.03, -0.04)^2))
  expect_error(ma_next_variance(returns, 5))
})

test_that("daily tracking re-runs the procedure on each session with the data up to it", {
  set.seed(37)
  returns <- c(rnorm(300, sd = 0.01), rnorm(60, sd = 0.03))
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = length(returns))
  windows <- c(5, 10, 20, 40)

  tracking <- ma_daily_tracking(returns, dates, from = dates[341],
                                windows = windows, train_share = 0.7)
  daily <- tracking$daily

  expect_equal(nrow(daily), 20)
  expect_equal(daily$date[1], dates[341])
  # The cut moves with the sample size.
  expect_equal(daily$n, 341:360)
  expect_equal(daily$cut_date, dates[floor(0.7 * (341:360))])
  expect_equal(daily$test_start, dates[floor(0.7 * (341:360)) + 1])

  # The last session reproduces a one-off run on the whole series.
  final_losses <- ma_window_losses(returns, windows, 0.7)
  last_rows <- tracking$losses[tracking$losses$date == dates[360], ]
  expect_equal(last_rows$rmse, final_losses$rmse)
  expect_equal(daily$chosen[20], ma_select_window(final_losses))
  expect_equal(daily$vol_chosen[20], sqrt(ma_next_variance(returns, daily$chosen[20])))
  expect_equal(daily$vol_m40[20], sqrt(mean(tail(returns, 40)^2)))

  # An earlier session ignores everything after it.
  first_losses <- ma_window_losses(returns[1:341], windows, 0.7)
  expect_equal(tracking$losses$mae[tracking$losses$date == dates[341]], first_losses$mae)

  expect_error(ma_daily_tracking(returns, dates, from = dates[360] + 1), "no session")
})
