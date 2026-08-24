# 04 — Derived datasets
#
# Turns the ingested panels into the analysis-ready tables the reports read.
# Pure transformation: no network calls, so it reruns offline and is the step to
# repeat after editing anything in R/returns.R.
#
#   Rscript analysis/04_build_datasets.R

source(file.path("R", "utils_io.R"))
source_lib()

main <- function() {
  prices <- read_processed("prices_daily")
  validate_prices(prices)

  returns <- compute_log_returns(prices)
  # Eight decimals on a daily return near 1e-2 is well past any precision this
  # analysis can claim, and keeps the committed file small.
  returns$log_return <- round(returns$log_return, 8)
  write_processed(returns, "returns_daily")

  summary_by_ticker <- do.call(
    rbind,
    lapply(split(returns, returns$ticker), function(one) {
      cbind(
        ticker = unique(one$ticker),
        first = format(min(one$date)),
        last = format(max(one$date)),
        return_moments(one$log_return)
      )
    })
  )
  rownames(summary_by_ticker) <- NULL

  message("\nFull-sample moments by asset:")
  print(
    within(summary_by_ticker, {
      mean_annual <- round(100 * mean_annual, 2)
      vol_annual <- round(100 * vol_annual, 2)
      mean_daily <- round(100 * mean_daily, 4)
      vol_daily <- round(100 * vol_daily, 3)
    }),
    row.names = FALSE
  )

  invisible(returns)
}

main()
