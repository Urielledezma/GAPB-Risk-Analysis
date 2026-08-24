# 01 — Daily price ingestion
#
# Downloads the full quotation history for every asset in config/assets.yml and
# writes the validated panel to data/processed/prices_daily.csv.
#
# Requires a network connection. Requires no credentials.
#
#   Rscript analysis/01_ingest_prices.R

source(file.path("R", "utils_io.R"))
source_lib()

library(quantmod)

main <- function() {
  message("Universe: ", paste(tickers(), collapse = ", "))
  message("History from: ", assets_config()$history_start)

  prices <- fetch_universe_prices()
  prices <- drop_unusable_prices(prices)
  prices <- round_prices(prices)
  validate_prices(prices)

  write_processed(prices, "prices_daily")

  coverage <- stats::aggregate(
    date ~ ticker,
    data = prices,
    FUN = function(d) paste(min(d), "to", max(d))
  )
  names(coverage) <- c("ticker", "coverage")
  print(coverage, row.names = FALSE)

  invisible(prices)
}

main()
