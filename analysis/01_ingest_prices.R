# 01 — Daily price ingestion
#
# Runs the source ladder in R/data_sources.R: FactSet first, the public source
# only if FactSet cannot answer. Where the result is written depends on where it
# came from, and that routing is the whole point of this script:
#
#   any FactSet rows  ->  data/private/  (untracked, licence-restricted)
#   public rows only  ->  data/processed/ (tracked, the reproducibility floor)
#
# By default it fetches the subject asset alone, which is all stages 01 to 04
# read. Pass the full universe when preparing stage 05:
#
#   Rscript analysis/01_ingest_prices.R              # GAPB only
#   Rscript analysis/01_ingest_prices.R --universe   # all six, for stage 05

source(file.path("R", "utils_io.R"))
source_lib()

library(quantmod)
library(httr2)

main <- function(symbols = NULL) {
  load_env()

  if (is.null(symbols)) {
    symbols <- if ("--universe" %in% commandArgs(trailingOnly = TRUE)) {
      tickers()
    } else {
      subject()
    }
  }

  message("Requesting: ", paste(symbols, collapse = ", "))
  message(
    "Source ladder: ", assets_config()$sources$primary,
    " then ", assets_config()$sources$fallback
  )

  prices <- fetch_prices_for(symbols)
  prices <- drop_unusable_prices(prices)
  prices <- round_prices(prices)
  validate_prices(prices)

  message("")
  manifest <- record_provenance(prices)

  # Licence, not processing stage, decides the destination.
  if (any(prices$source == "factset")) {
    write_private(prices, "prices_daily")
    message(
      "\nVendor data is not committed. The tracked snapshot in data/processed/ ",
      "is refreshed separately by analysis/05_refresh_public_snapshot.R."
    )
  } else {
    write_processed(prices, "prices_daily")
  }

  invisible(list(prices = prices, provenance = manifest))
}

main()
