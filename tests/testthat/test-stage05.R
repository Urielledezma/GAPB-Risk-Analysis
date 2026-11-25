test_that("historical blocks keep the session calendar and the initial positions", {
  panel <- matrix(c(10, 20, 10, 10, 10, NA, 20, 20), ncol = 2,
                  dimnames = list(as.character(as.Date("2026-01-01") + 0:3), c("A", "B")))
  one <- stage05_asset_scenarios(panel, 1L)
  expect_equal(one$date, as.Date("2026-01-04"))
  two <- stage05_asset_scenarios(panel, 2L)
  expect_equal(two$date, as.Date("2026-01-03"))
  expect_equal(unname(two$returns[1, ]), c(0, 1))
  expect_equal(mean(two$returns[1, ]), 0.5)
})

test_that("months are measured on the calendar, including short months", {
  expect_equal(stage05_months_before(as.Date("2024-08-31"), 6), as.Date("2024-02-29"))
  expect_equal(stage05_months_before(as.Date("2026-11-23"), 6), as.Date("2026-05-23"))
})

test_that("expected shortfall includes exactly the specified probability mass", {
  expect_equal(stage05_es(c(20, 10, 10, 10, 0), .6), 15)
  expect_equal(stage05_es(c(30, 15, 0), .5), 25)
  expect_equal(stage05_es(c(30, 15, 0), .99), 30)
})

test_that("the public portfolio reconciles to positions and ex ante forecasts", {
  prices <- read_processed("stage05_prices")
  p <- stage05_portfolio(prices)
  expect_equal(p$value, 600000)
  expect_equal(p$holdings$shares * p$holdings$price, rep(100000, 6))
  expect_equal(nrow(p$risk), 12L)
  expect_true(all(p$risk$historical_es_mxn >= p$risk$historical_var_mxn))
  expect_true(all(p$backtest$train_last < p$backtest$date))
  idx <- which.max(-p$daily$pnl_mxn[p$daily$date >= as.Date("2026-01-01")])
  target <- sort(unique(p$backtest$date))[idx]
  b <- subset(p$backtest, date == target & method == "historical" & confidence == .99)
  before <- p$daily$pnl_mxn[p$daily$date < target]
  expect_equal(b$var_mxn, -unname(quantile(before, .01)))
  expect_equal(b$breach, -b$pnl_mxn > b$var_mxn)
  # A Monday forecast uses the six calendar months known at Friday's close.
  monday <- as.Date("2026-11-23")
  friday <- as.Date("2026-11-20")
  window <- p$daily$date > as.Date("2026-05-20") & p$daily$date <= friday
  normal <- subset(p$backtest, date == monday & method == "normal" & confidence == .99)
  expect_equal(normal$train_last, friday)
  expect_equal(normal$train_n, sum(window))
  expect_equal(normal$var_mxn, p$value * qnorm(.99) * sd(p$daily$portfolio_return[window]))
  expect_equal(p$coverage$observed_breaches, c(8, 7, 5, 6, 1, 3))
})
