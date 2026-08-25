# The source ladder decides where every number in every report comes from, so
# the parts of it that can be tested without a network are tested here.

test_that("the configured ladder puts FactSet first", {
  sources <- assets_config()$sources

  expect_equal(sources$primary, "factset")
  expect_equal(sources$fallback, "yahoo")
  expect_true(is.logical(sources$allow_fallback))
})

test_that("both loaders conform to one schema", {
  factset_shaped <- data.frame(
    date = as.Date("2024-01-02"), ticker = "X", open = 1, high = 2,
    low = 0.5, close = 1.5, adjusted = 1.5, volume = 100, source = "factset"
  )
  # A frame missing columns must still conform rather than error.
  sparse <- data.frame(
    date = as.Date("2024-01-02"), ticker = "X", close = 1.5,
    adjusted = 1.5, source = "yahoo"
  )

  expect_equal(names(conform_price_schema(factset_shaped)), PRICE_SCHEMA)
  expect_equal(names(conform_price_schema(sparse)), PRICE_SCHEMA)
  expect_true(all(is.na(conform_price_schema(sparse)$high)))
})

test_that("conforming reorders rather than reshuffles values", {
  scrambled <- data.frame(
    source = "factset", volume = 100, adjusted = 1.5, close = 1.5,
    low = 0.5, high = 2, open = 1, ticker = "X", date = as.Date("2024-01-02")
  )

  conformed <- conform_price_schema(scrambled)

  expect_equal(conformed$open, 1)
  expect_equal(conformed$volume, 100)
  expect_equal(conformed$source, "factset")
})

test_that("provenance counts rows per ticker and source", {
  prices <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    ticker = rep(c("A", "B"), each = 5),
    source = c(rep("factset", 5), rep("yahoo", 5)),
    stringsAsFactors = FALSE
  )

  manifest <- provenance(prices)

  expect_equal(nrow(manifest), 2)
  expect_equal(manifest$rows, c(5, 5))
  expect_equal(sort(manifest$source), c("factset", "yahoo"))
})

test_that("pick_field takes the first present candidate", {
  row <- list(priceClose = 42, currency = "MXN")

  expect_equal(pick_field(row, c("price", "priceClose", "close")), 42)
  expect_equal(pick_field(row, c("currency", "currencyCode")), "MXN")
  # Absent everywhere: the default, not an error, so one missing field does not
  # take down a whole ingest.
  expect_true(is.na(pick_field(row, c("volume", "tradeVolume"), NA_real_)))
})

test_that("log returns carry provenance across from the prices", {
  prices <- data.frame(
    date = as.Date("2024-01-01") + 0:4,
    ticker = "A",
    adjusted = c(100, 101, 102, 103, 104),
    source = "factset",
    stringsAsFactors = FALSE
  )

  returns <- compute_log_returns(prices)

  expect_true("source" %in% names(returns))
  expect_true(all(returns$source == "factset"))
  expect_equal(nrow(returns), 4)
})

test_that("a price frame without provenance still computes returns", {
  prices <- data.frame(
    date = as.Date("2024-01-01") + 0:4,
    ticker = "A",
    adjusted = c(100, 101, 102, 103, 104),
    stringsAsFactors = FALSE
  )

  returns <- compute_log_returns(prices)

  expect_false("source" %in% names(returns))
  expect_equal(nrow(returns), 4)
})
