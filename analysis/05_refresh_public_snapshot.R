# 05 — Refresh the tracked public snapshot
#
# Deliberately bypasses the source ladder and pulls the public source only. Its
# output is the tracked dataset in data/processed/ -- the floor that keeps the
# repository renderable for a reader with no FactSet key, and the baseline the
# vendor series can be checked against.
#
# Run it when extending the coverage window, not on every ingest. The committed
# snapshot changing on every run would make every pull request a data diff.
#
# Always covers the full universe: the snapshot is the floor for stage 05 as
# well, and a partial floor is not one.
#
#   Rscript analysis/05_refresh_public_snapshot.R

source(file.path("R", "utils_io.R"))
source_lib()

library(quantmod)

main <- function() {
  symbols <- tickers()
  message("Public snapshot for: ", paste(symbols, collapse = ", "))

  collected <- list()
  for (ticker in symbols) {
    message("Fetching ", ticker, " ...")
    result <- tryCatch(
      fetch_prices_yahoo(ticker),
      error = function(e) {
        warning("  failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) {
      message("  ", nrow(result), " rows")
      collected[[ticker]] <- conform_price_schema(result)
    }
  }

  if (length(collected) == 0) {
    stop("The public source returned nothing.", call. = FALSE)
  }

  prices <- do.call(rbind, collected)
  rownames(prices) <- NULL
  prices <- prices[order(prices$ticker, prices$date), ]

  prices <- drop_unusable_prices(prices)
  prices <- round_prices(prices)
  validate_prices(prices)

  write_processed(prices, "prices_daily")
  write_processed(provenance(prices), "prices_provenance")

  invisible(prices)
}

main()
