# Archived from R/portfolio.R on 2026-11-24.
# Stage 05 uses fixed-holdings scenario P&L and calendar-month covariance.
# Report 05 solves for minimum normal VaR under its weight constraints, then
# compares allocations. The optimiser below targeted historical VaR instead.

#' Portfolio log returns from an asset return matrix.
#'
#' Weights apply to simple returns, so log returns are converted, weighted and
#' converted back. Weighting log returns directly is a common shortcut and it is
#' wrong: a portfolio of assets is a linear combination of prices, not of their
#' logarithms.
#'
#' @param matrix_returns A date-by-ticker matrix of log returns.
#' @param weights Named weight vector.
#' @return A numeric vector of portfolio log returns, named by date.
portfolio_returns <- function(matrix_returns, weights) {
  weights <- weights[colnames(matrix_returns)]
  simple <- exp(matrix_returns) - 1
  portfolio_simple <- as.numeric(simple %*% as.numeric(weights))
  out <- log1p(portfolio_simple)
  names(out) <- rownames(matrix_returns)
  out
}

#' Covariance matrix of asset returns.
#'
#' @param matrix_returns A date-by-ticker matrix of log returns.
#' @param months Trailing window in months. NULL uses the whole sample.
#' @return A covariance matrix.
covariance_matrix <- function(matrix_returns,
                              months = params()$var$parametric_window_months) {
  if (!is.null(months)) {
    dates <- as.Date(rownames(matrix_returns))
    cutoff <- max(dates) - months * 30.44
    matrix_returns <- matrix_returns[dates >= cutoff, , drop = FALSE]
  }
  stats::cov(matrix_returns)
}

#' Weights that minimise portfolio VaR.
#'
#' Minimises the historical-simulation VaR directly rather than minimising
#' variance and hoping VaR follows. The two coincide only under normality, and
#' the whole point of stage 05 is that returns are not normal.
#'
#' @param matrix_returns A date-by-ticker matrix of log returns.
#' @param confidence Confidence level.
#' @param long_only Forbid short positions.
#' @param max_weight Maximum weight in any single asset.
#' @return A list with the optimal weights, the achieved VaR return and the
#'   optimiser's convergence status.
optimise_min_var <- function(matrix_returns,
                             confidence = params()$var$optimisation$confidence,
                             long_only = params()$var$optimisation$long_only,
                             max_weight = params()$var$optimisation$max_weight) {
  if (!requireNamespace("nloptr", quietly = TRUE)) {
    stop("Package 'nloptr' is required. Run renv::restore().", call. = FALSE)
  }

  assets <- colnames(matrix_returns)
  n <- length(assets)
  simple <- exp(matrix_returns) - 1

  objective <- function(w) {
    portfolio_simple <- as.numeric(simple %*% w)
    # Negated so that a smaller loss quantile is a smaller objective value.
    -stats::quantile(log1p(portfolio_simple), probs = 1 - confidence, names = FALSE)
  }

  result <- nloptr::slsqp(
    x0 = rep(1 / n, n),
    fn = objective,
    lower = if (long_only) rep(0, n) else rep(-max_weight, n),
    upper = rep(max_weight, n),
    heq = function(w) sum(w) - 1
  )

  weights <- result$par
  names(weights) <- assets

  list(
    weights = weights,
    var_return = -result$value,
    converged = result$convergence >= 0,
    iterations = result$iter
  )
}

#' Compare two allocations on risk and concentration.
#'
#' The diversification benefit is the gap between the sum of the standalone
#' VaRs and the VaR of the combined book. It is the part of the risk that
#' correlation removes, and it is what makes a portfolio worth holding.
#'
#' @param matrix_returns A date-by-ticker matrix of log returns.
#' @param allocations A named list of weight vectors.
#' @param value Portfolio value.
#' @param confidence Confidence level.
#' @return One row per allocation: VaR, volatility, effective number of assets
#'   and the diversification benefit.
compare_allocations <- function(matrix_returns, allocations, value,
                                confidence = params()$var$optimisation$confidence) {
  covariance <- stats::cov(matrix_returns)

  standalone <- vapply(colnames(matrix_returns), function(asset) {
    var_historical(
      matrix_returns[, asset], confidence,
      value = value / ncol(matrix_returns)
    )$var_currency
  }, numeric(1))

  rows <- lapply(names(allocations), function(name) {
    weights <- allocations[[name]]
    returns <- portfolio_returns(matrix_returns, weights)
    portfolio_var <- var_historical(returns, confidence, value = value)$var_currency

    data.frame(
      allocation = name,
      var_currency = portfolio_var,
      var_pct = portfolio_var / value,
      sigma_daily = portfolio_sigma(weights, covariance),
      # Inverse Herfindahl: how many equally weighted assets this book behaves
      # like. Six means fully spread, one means a single-name bet.
      effective_assets = 1 / sum(weights^2),
      max_weight = max(weights),
      diversification_benefit = sum(standalone) - portfolio_var,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
