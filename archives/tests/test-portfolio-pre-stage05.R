# Archived from tests/testthat/test-portfolio.R on 2026-11-24.
# Tests of archives/R/portfolio_pre_stage05.R. They use the active portfolio
# test fixture make_matrix() and the archived var_historical() helper.

test_that("portfolio returns aggregate simple returns, not log returns", {
  matrix_returns <- matrix(log(c(1.10, 0.90)), nrow = 1)
  colnames(matrix_returns) <- c("A", "B")
  rownames(matrix_returns) <- "2024-01-02"
  weights <- c(A = 0.5, B = 0.5)

  result <- portfolio_returns(matrix_returns, weights)

  # A book half up 10% and half down 10% is flat, not exp(0) via averaged logs.
  expect_equal(as.numeric(result), 0, tolerance = 1e-12)
  # The naive shortcut would also give zero here, so check an asymmetric case.
  weights_skewed <- c(A = 0.8, B = 0.2)
  expected_simple <- 0.8 * 0.10 + 0.2 * -0.10
  expect_equal(
    as.numeric(portfolio_returns(matrix_returns, weights_skewed)),
    log1p(expected_simple)
  )
})

test_that("minimum-VaR weights are feasible and no worse than equal weights", {
  skip_if_not_installed("nloptr")
  matrix_returns <- make_matrix(n = 1000)

  result <- optimise_min_var(
    matrix_returns, confidence = 0.99, long_only = TRUE, max_weight = 0.6
  )

  expect_equal(sum(result$weights), 1, tolerance = 1e-6)
  expect_true(all(result$weights >= -1e-8))
  expect_true(all(result$weights <= 0.6 + 1e-8))

  equal <- rep(1 / 3, 3)
  names(equal) <- colnames(matrix_returns)
  equal_var <- -quantile(portfolio_returns(matrix_returns, equal), 0.01, names = FALSE)

  expect_lte(result$var_return, equal_var + 1e-6)
})

test_that("compare_allocations reports concentration and diversification", {
  matrix_returns <- make_matrix()
  equal <- c(A = 1 / 3, B = 1 / 3, C = 1 / 3)
  concentrated <- c(A = 0.9, B = 0.05, C = 0.05)

  comparison <- compare_allocations(
    matrix_returns,
    list(equal = equal, concentrated = concentrated),
    value = 300000
  )

  expect_equal(nrow(comparison), 2)
  # Inverse Herfindahl: three for a fully spread book, near one for a single bet.
  expect_equal(comparison$effective_assets[1], 3, tolerance = 1e-8)
  expect_lt(comparison$effective_assets[2], 1.3)
  expect_gt(comparison$diversification_benefit[1], comparison$diversification_benefit[2])
})
