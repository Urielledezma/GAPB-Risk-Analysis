# Public equity snapshot for Part 5. Run from the project root.
# The analysis date is exclusive so the current intraday bar never enters.
source(file.path("R", "utils_io.R"))
source_lib()
as_of <- as.Date("2026-11-24")
symbols <- tickers()
panels <- lapply(symbols, fetch_prices_yahoo, to = as_of)
prices <- do.call(rbind, panels)
prices <- prices[prices$date < as_of, c("date", "ticker", "close", "adjusted")]
prices <- prices[order(prices$ticker, prices$date), ]
valid <- with(prices, is.finite(close) & close > 0 & is.finite(adjusted) & adjusted > 0)
last_dates <- tapply(as.character(prices$date[valid]), prices$ticker[valid], max)
stopifnot(!anyDuplicated(prices[c("ticker", "date")]), length(last_dates) == length(symbols),
          all(last_dates == "2026-11-23"))
write_processed(prices, "stage05_prices")
