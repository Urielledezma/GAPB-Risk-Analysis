# These tests guard the configuration contract. Every ticker, window and
# confidence level in the project comes from config/, so a typo there is a wrong
# number in a report rather than an error.

test_that("the universe parses and the subject belongs to it", {
  assets <- universe()

  expect_gt(nrow(assets), 0)
  expect_true(all(c("ticker", "name", "sector", "factset_id") %in% names(assets)))
  expect_true(subject() %in% assets$ticker)
  expect_false(any(duplicated(assets$ticker)))
})

test_that("asset_meta rejects an unknown ticker with a listing of the known ones", {
  expect_error(asset_meta("NOT.A.TICKER"), "Unknown ticker")
  expect_equal(nrow(asset_meta(subject())), 1)
})

test_that("model parameters are present and internally consistent", {
  p <- params()

  expect_true(as.Date(p$sample$start) < as.Date(p$sample$end))
  expect_true(as.Date(p$sample$oos_start) > as.Date(p$sample$end))
  expect_equal(p$conventions$trading_days_per_year, 252)
  expect_equal(p$conventions$return_type, "log")

  expect_true(all(p$var$confidence > 0.5 & p$var$confidence < 1))
  expect_true(all(p$var$horizons_days > 0))
  expect_true(p$var$position_mxn > 0)
  expect_true(p$var$optimisation$max_weight > 1 / nrow(universe()))
})

test_that("the EWMA grid is a valid set of decay factors and contains RiskMetrics", {
  grid <- ewma_lambda_grid()

  expect_true(all(grid > 0 & grid < 1))
  expect_true(any(abs(grid - params()$variance$ewma$lambda_riskmetrics) < 1e-9))
})

test_that("the palette has a slot for every asset and refuses to invent one", {
  colours <- asset_colours()

  expect_equal(length(colours), nrow(universe()))
  expect_equal(colours[[1]], RISK_PALETTE[1])
  # The subject asset always takes slot one.
  expect_equal(names(colours)[1], subject())
  expect_false(any(duplicated(colours)))

  expect_error(
    asset_colours(paste0("T", seq_len(length(RISK_PALETTE) + 1))),
    "palette has"
  )
})

test_that("read_processed points at the ingest pipeline when a dataset is missing", {
  expect_error(read_processed("no_such_dataset"), "Run the ingest pipeline")
})

test_that("the seed is applied and reproduces draws", {
  use_seed()
  first <- runif(5)
  use_seed()
  second <- runif(5)

  expect_identical(first, second)
})
