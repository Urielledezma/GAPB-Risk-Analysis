# Distributional tests.
#
# The statistics are written out rather than delegated to a package, because
# stage 03 has to show the equation it is applying and a reader has to be able
# to follow the arithmetic from the sample to the p-value. The implementations
# are checked against tseries and moments in the unit tests.

#' Sample skewness.
#'
#'   S = (1/n) * sum((x - xbar)^3) / sigma^3
#'
#' Population (biased) convention, which is the one the Jarque-Bera statistic
#' assumes.
#'
#' @param x Numeric vector.
#' @return The skewness.
skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  centred <- x - mean(x)
  sigma <- sqrt(sum(centred^2) / n)
  (sum(centred^3) / n) / sigma^3
}

#' Sample kurtosis.
#'
#'   K = (1/n) * sum((x - xbar)^4) / sigma^4
#'
#' @param x Numeric vector.
#' @param excess Return excess kurtosis, K - 3, instead of K.
#' @return The kurtosis.
kurtosis <- function(x, excess = FALSE) {
  x <- x[is.finite(x)]
  n <- length(x)
  centred <- x - mean(x)
  sigma <- sqrt(sum(centred^2) / n)
  k <- (sum(centred^4) / n) / sigma^4
  if (excess) k - 3 else k
}

#' Jarque-Bera test of normality.
#'
#'   JB = (n / 6) * (S^2 + (K - 3)^2 / 4)   ~   chi-squared with 2 df
#'
#' The statistic is a joint test on the third and fourth moments: it asks
#' whether the sample is too skewed, too fat-tailed, or both, to have come from
#' a normal distribution.
#'
#' @param x Numeric vector.
#' @param alpha Significance level.
#' @return A one-row data frame: n, skewness, kurtosis, excess kurtosis, the
#'   statistic, its p-value and the decision.
jarque_bera <- function(x, alpha = params()$tests$alpha) {
  x <- x[is.finite(x)]
  n <- length(x)
  s <- skewness(x)
  k <- kurtosis(x)
  statistic <- (n / 6) * (s^2 + ((k - 3)^2) / 4)
  p_value <- stats::pchisq(statistic, df = 2, lower.tail = FALSE)

  data.frame(
    n = n,
    skewness = s,
    kurtosis = k,
    excess_kurtosis = k - 3,
    statistic = statistic,
    p_value = p_value,
    reject_normality = p_value < alpha,
    stringsAsFactors = FALSE
  )
}

#' Jarque-Bera applied to several series at once.
#'
#' Stage 03 runs it on returns, on the price level and on the log price, because
#' the geometric Brownian motion assumption is a claim about all three: returns
#' normal, price lognormal, log price normal.
#'
#' @param series A named list of numeric vectors.
#' @param alpha Significance level.
#' @return One row per series, with the series name in the first column.
jarque_bera_table <- function(series, alpha = params()$tests$alpha) {
  rows <- lapply(names(series), function(name) {
    cbind(series = name, jarque_bera(series[[name]], alpha = alpha))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' One-sample t-test that the mean return is zero.
#'
#'   t = xbar / (s / sqrt(n))   ~   t with n - 1 df
#'
#' Failing to reject is the interesting outcome: it is what a market with no
#' exploitable drift at daily frequency looks like.
#'
#' @param x Numeric vector.
#' @param mu Null hypothesis value.
#' @param alpha Significance level.
#' @return A one-row data frame with the estimate, standard error, statistic,
#'   degrees of freedom, p-value, confidence interval and decision.
mean_zero_test <- function(x, mu = 0, alpha = params()$tests$alpha) {
  x <- x[is.finite(x)]
  n <- length(x)
  estimate <- mean(x)
  standard_error <- stats::sd(x) / sqrt(n)
  statistic <- (estimate - mu) / standard_error
  df <- n - 1
  p_value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
  critical <- stats::qt(1 - alpha / 2, df = df)

  data.frame(
    n = n,
    mean = estimate,
    std_error = standard_error,
    statistic = statistic,
    df = df,
    p_value = p_value,
    conf_low = estimate - critical * standard_error,
    conf_high = estimate + critical * standard_error,
    reject_zero_mean = p_value < alpha,
    stringsAsFactors = FALSE
  )
}

#' Descriptive statistics for a return series.
#'
#' @param x Numeric vector.
#' @return A one-row data frame of the moments and order statistics a risk
#'   report normally opens with.
describe <- function(x) {
  x <- x[is.finite(x)]
  quantiles <- stats::quantile(x, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))
  data.frame(
    n = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    q01 = quantiles[[1]],
    q05 = quantiles[[2]],
    q25 = quantiles[[3]],
    median = quantiles[[4]],
    q75 = quantiles[[5]],
    q95 = quantiles[[6]],
    q99 = quantiles[[7]],
    max = max(x),
    skewness = skewness(x),
    excess_kurtosis = kurtosis(x, excess = TRUE),
    stringsAsFactors = FALSE
  )
}
