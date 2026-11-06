# Moving-average variance and the class procedure for choosing m (Modulo 3).
#
#   MA(m)   sigma^2_t = (1/m) * sum_{i=1..m} r^2_{t-i}
#
# The other class models -- EWMA by maximum likelihood, the data-driven EWMA
# and ARCH/GARCH -- live in vol_class.R. An earlier generation of EWMA and
# GARCH helpers, written before the class procedures were adopted, is kept in
# archives/R/vol_models_pre_class.R.
#
# Variance is estimated from squared returns rather than from deviations
# around a rolling mean. At daily frequency the mean return is indistinguishable
# from zero -- stage 03 tests exactly that -- and subtracting a noisy estimate of
# it adds variance to the estimator without removing bias.
#
# Every estimate at time t uses information up to t-1 only. A volatility model
# that peeks at the contemporaneous return will look excellent and forecast
# nothing.

#' Moving-average volatility.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param m Window length in observations.
#' @return A numeric vector the same length as returns. The first m entries are
#'   NA, having no full window behind them.
ma_volatility <- function(returns, m) {
  n <- length(returns)
  squared <- returns^2
  sigma2 <- rep(NA_real_, n)
  if (n <= m) {
    return(sqrt(sigma2))
  }
  for (t in seq.int(m + 1, n)) {
    sigma2[t] <- mean(squared[seq.int(t - m, t - 1)])
  }
  sqrt(sigma2)
}

#' Trailing moving-average volatility, the descriptive measure used in class.
#'
#' The window ends at t and includes it,
#'
#'   sigma^2_t = (1/m) * sum_{i=0..m-1} r^2_{t-i},
#'
#' which is SMA(r^2, n = m) in the class notebooks. It describes the m sessions
#' up to t. ma_volatility() is the same average shifted one session, the
#' forecast for the session after it.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param m Window length in observations.
#' @return A numeric vector the same length as returns. The first m - 1
#'   entries are NA.
ma_trailing_volatility <- function(returns, m) {
  if (length(returns) < m) {
    return(rep(NA_real_, length(returns)))
  }
  c(ma_volatility(returns, m)[-1], sqrt(ma_next_variance(returns, m)))
}

#' Moving-average volatility across a grid of windows.
#'
#' @param returns Numeric vector of log returns.
#' @param windows Window lengths to sweep.
#' @return A matrix, one column per window.
ma_volatility_grid <- function(returns, windows = params()$variance$ma_class$windows) {
  out <- vapply(windows, function(m) ma_volatility(returns, m), numeric(length(returns)))
  colnames(out) <- paste0("m_", windows)
  out
}

# --- The class procedure for choosing m (Modulo 3) ---------------------------
#
# Split the sample 70 / 30 at floor(0.7 * n); forecast the variance of each
# evaluation day t with the mean of the m squared returns before it; score the
# forecasts against r_t^2 with four losses; keep the window that wins most of
# them; and forecast the next session with the mean of the last m squared
# returns. Stage 03 re-runs it on every session, so the cut, the scores, the
# chosen window and the forecast all move as data arrives.

#' The four forecast losses used in class.
#'
#' Definitions are the class notebook's, unchanged:
#'
#'   RMSE     = sqrt(mean((y - f)^2))
#'   MAE      = mean(|y - f|)
#'   QLIKE    = mean(f / y - ln(f / y) - 1)
#'   relative = mean(|1 - y / f|)
#'
#' with y the squared return and f its forecast. A day with a zero return makes
#' f / y infinite and its QLIKE term NaN, and na.rm = TRUE drops it, exactly as
#' the class code does.
#'
#' @param realised Squared returns.
#' @param forecast Variance forecasts aligned with realised.
#' @return A named numeric vector: rmse, mae, qlike, relative.
ma_class_losses <- function(realised, forecast) {
  ratio <- forecast / realised
  c(
    rmse = sqrt(mean((realised - forecast)^2, na.rm = TRUE)),
    mae = mean(abs(realised - forecast), na.rm = TRUE),
    qlike = mean(ratio - log(ratio) - 1, na.rm = TRUE),
    relative = mean(abs(1 - realised / forecast), na.rm = TRUE)
  )
}

#' Choose the moving-average window as in class.
#'
#' Each loss votes for the window that minimises it and the window with most
#' votes wins. In class the window kept was the one that minimised RMSE and MAE
#' at once, so a tie goes to the tied window with the lowest RMSE.
#'
#' @param losses A data frame with a column m and one column per loss, as
#'   returned by ma_window_losses().
#' @return The chosen window.
ma_select_window <- function(losses) {
  criteria <- c("rmse", "mae", "qlike", "relative")
  winners <- vapply(criteria, function(k) {
    best <- which.min(losses[[k]])
    if (length(best) == 0) NA_real_ else losses$m[best]
  }, numeric(1))
  votes <- table(factor(winners, levels = losses$m))
  tied <- losses[votes == max(votes), , drop = FALSE]
  tied$m[which.min(tied$rmse)]
}

#' Score every candidate window on the evaluation part of one sample.
#'
#' Forecasts are computed once on the full series and then cut, which is
#' equivalent to recomputing them on the truncated sample because the forecast
#' for day t only uses returns before t.
#'
#' @param realised Squared returns, in time order.
#' @param forecasts Matrix of variance forecasts, one column per window.
#' @param windows Window lengths, one per column.
#' @param last Index of the last observation in the sample.
#' @param train_share Share of the sample before the cut.
#' @return One row per window: m, n_test and the four losses.
ma_losses_at <- function(realised, forecasts, windows, last, train_share) {
  cut <- floor(train_share * last)
  test <- seq.int(cut + 1, last)
  rows <- lapply(seq_along(windows), function(k) {
    keep <- test[!is.na(forecasts[test, k])]
    as.data.frame(as.list(c(m = windows[k], n_test = length(keep),
                            ma_class_losses(realised[keep], forecasts[keep, k]))))
  })
  do.call(rbind, rows)
}

#' Out-of-sample losses of each moving-average window, class procedure.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param windows Candidate windows.
#' @param train_share Share of the sample before the cut.
#' @return One row per window: m, n_test, rmse, mae, qlike and relative.
ma_window_losses <- function(returns,
                             windows = params()$variance$ma_class$windows,
                             train_share = params()$variance$ma_class$train_share) {
  forecasts <- ma_volatility_grid(returns, windows)^2
  ma_losses_at(returns^2, forecasts, windows, length(returns), train_share)
}

#' Next-session variance forecast: the mean of the last m squared returns.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param m Window length.
#' @return The variance forecast for the session after the last observation.
ma_next_variance <- function(returns, m) {
  stopifnot(length(returns) >= m)
  mean(utils::tail(returns, m)^2)
}

#' Re-run the class procedure on every session from a date onwards.
#'
#' On each tracked session the procedure sees only the returns up to that
#' session: the 70 / 30 cut moves with the sample, every window is scored
#' again, the window is chosen again and the next-session variance is
#' forecast with it.
#'
#' @param returns Numeric vector of log returns, in time order.
#' @param dates Dates aligned with returns.
#' @param from First session to track.
#' @param windows Candidate windows.
#' @param train_share Share of the sample before the cut.
#' @return A list. daily: one row per tracked session with date, n, cut_date,
#'   test_start, n_test, chosen, the next-session volatility of every window
#'   (vol_m<m>) and of the chosen one (vol_chosen). losses: one row per
#'   session and window with the four losses and whether the window was chosen.
ma_daily_tracking <- function(returns, dates, from,
                              windows = params()$variance$ma_class$windows,
                              train_share = params()$variance$ma_class$train_share) {
  stopifnot(length(returns) == length(dates), !is.unsorted(dates))
  realised <- returns^2
  forecasts <- ma_volatility_grid(returns, windows)^2
  tracked <- which(dates >= as.Date(from))
  if (length(tracked) == 0) {
    stop("ma_daily_tracking: no session on or after ", format(as.Date(from)), call. = FALSE)
  }

  daily <- vector("list", length(tracked))
  losses <- vector("list", length(tracked))
  for (j in seq_along(tracked)) {
    last <- tracked[j]
    cut <- floor(train_share * last)
    scores <- ma_losses_at(realised, forecasts, windows, last, train_share)
    chosen <- ma_select_window(scores)
    next_vol <- vapply(windows, function(m) sqrt(ma_next_variance(returns[seq_len(last)], m)),
                       numeric(1))

    day <- data.frame(date = dates[last], n = last, cut_date = dates[cut],
                      test_start = dates[cut + 1], n_test = last - cut, chosen = chosen)
    day[paste0("vol_m", windows)] <- as.list(next_vol)
    day$vol_chosen <- next_vol[windows == chosen]
    daily[[j]] <- day

    scores$date <- dates[last]
    scores$chosen <- scores$m == chosen
    losses[[j]] <- scores
  }

  list(daily = do.call(rbind, daily), losses = do.call(rbind, losses))
}
