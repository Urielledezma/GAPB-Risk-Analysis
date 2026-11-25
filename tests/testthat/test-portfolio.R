make_matrix <- function(n = 500, seed = 73) {
  set.seed(seed)
  matrix_returns <- matrix(rnorm(n * 3, sd = 0.02), ncol = 3)
  colnames(matrix_returns) <- c("A", "B", "C")
  rownames(matrix_returns) <- format(as.Date("2020-01-01") + seq_len(n) - 1)
  matrix_returns
}

test_that("equal weights allocate equal currency, not equal share counts", {
  prices <- data.frame(
    date = rep(as.Date("2024-01-02"), 3),
    ticker = c("CHEAP", "MID", "DEAR"),
    adjusted = c(10, 100, 1000)
  )

  portfolio <- build_portfolio(prices, position = 100000)

  expect_equal(portfolio$value, 300000)
  expect_equal(portfolio$holdings$value, rep(100000, 3))

  # Holdings come back in ticker order, so look them up by name rather than
  # assuming the order they were supplied in.
  shares <- setNames(portfolio$holdings$shares, portfolio$holdings$ticker)
  # Equal money means very unequal share counts.
  expect_equal(shares[["CHEAP"]], 10000)
  expect_equal(shares[["MID"]], 1000)
  expect_equal(shares[["DEAR"]], 100)
})

test_that("the quoted price column determines shares at the valuation date", {
  prices <- data.frame(date = as.Date("2024-01-02"), ticker = c("B", "A"),
                       adjusted = c(10, 20), close = c(25, 50))
  default <- build_portfolio(prices, position = 100)
  quoted <- build_portfolio(prices, position = 100, price_col = "close")

  expect_equal(default$holdings$price, c(20, 10))
  expect_equal(quoted$holdings$price, c(50, 25))
  expect_equal(quoted$holdings$shares, c(2, 4))
  expect_equal(quoted$holdings$value, default$holdings$value)
})

test_that("portfolio volatility follows w' Sigma w", {
  matrix_returns <- make_matrix()
  covariance <- stats::cov(matrix_returns)
  weights <- c(A = 0.5, B = 0.3, C = 0.2)

  expect_equal(
    portfolio_sigma(weights, covariance),
    sqrt(as.numeric(t(weights) %*% covariance %*% weights))
  )
})

test_that("weights are matched by name, not by position", {
  matrix_returns <- make_matrix()
  covariance <- stats::cov(matrix_returns)

  ordered <- c(A = 0.5, B = 0.3, C = 0.2)
  shuffled <- c(C = 0.2, A = 0.5, B = 0.3)

  expect_equal(portfolio_sigma(ordered, covariance), portfolio_sigma(shuffled, covariance))
})

test_that("diversification reduces volatility below the weighted average", {
  matrix_returns <- make_matrix()
  covariance <- stats::cov(matrix_returns)
  weights <- c(A = 1 / 3, B = 1 / 3, C = 1 / 3)

  combined <- portfolio_sigma(weights, covariance)
  weighted_average <- sum(weights * sqrt(diag(covariance)))

  # The three series are independent by construction, so the gap must be large.
  expect_lt(combined, weighted_average)
})
