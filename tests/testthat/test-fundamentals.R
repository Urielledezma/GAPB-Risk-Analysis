#' A minimal annual row satisfying the report contract.
#'
#' @param ... Columns to override.
#' @return A one-row data frame.
annual_fixture <- function(...) {
  row <- data.frame(
    year = 2025L,
    revenue_reported_mxn_m = 41408.540,
    revenue_operating_mxn_m = 32525.907,
    cost_services_mxn_m = 6490.747,
    operating_income_mxn_m = 17580.115,
    depreciation_mxn_m = 3751.948,
    net_income_mxn_m = 10000.609,
    net_income_controlling_mxn_m = 9565.171,
    ebitda_mxn_m = 21332.063,
    total_assets_mxn_m = 88140.275,
    total_equity_mxn_m = 24835.931,
    equity_controlling_mxn_m = 22470.451,
    debt_current_mxn_m = 9024.828,
    debt_long_term_mxn_m = 43972.265,
    total_debt_mxn_m = 52997.093,
    cash_mxn_m = 10453.198,
    weighted_shares = 505277464,
    reported_eps_mxn = 18.9305,
    source = "GAP Form 20-F 2025",
    source_url = "https://example.com",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (column in names(overrides)) {
    row[[column]] <- overrides[[column]]
  }
  row
}

#' A minimal quarterly row satisfying the report contract.
#'
#' @param ... Columns to override.
#' @return A one-row data frame.
eps_fixture <- function(...) {
  row <- data.frame(
    period_end = as.Date("2025-12-31"),
    year = 2025L,
    quarter = "T4",
    diluted_eps = 3.5453,
    net_income_mxn_m = 1791.377,
    comprehensive_income_mxn_m = 1493.256,
    comprehensive_income_controlling_mxn_m = 1429.264,
    comprehensive_eps_controlling_mxn = 2.8287,
    shares_outstanding = 505277464,
    usdmxn = 18.0057,
    usdmxn_implied = 18.0059,
    reported_eps_mxn = 2.9553,
    reported_eps_usd_ads = 1.6413,
    currency = "MXN",
    instrument = "GAPB",
    source = "GAP 4Q25 results",
    source_url = "https://example.com",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (column in names(overrides)) {
    row[[column]] <- overrides[[column]]
  }
  row
}

test_that("annual fundamentals require the report contract", {
  valid <- annual_fixture()

  expect_invisible(validate_fundamentals_annual(valid))
  expect_error(
    validate_fundamentals_annual(valid[, setdiff(names(valid), "cost_services_mxn_m")]),
    "cost_services_mxn_m"
  )
  # The leverage columns were the gap this contract exists to close.
  expect_error(
    validate_fundamentals_annual(valid[, setdiff(names(valid), "total_debt_mxn_m")]),
    "total_debt_mxn_m"
  )
  expect_error(
    validate_fundamentals_annual(valid[, setdiff(names(valid), "equity_controlling_mxn_m")]),
    "equity_controlling_mxn_m"
  )
})

test_that("quarterly EPS rejects duplicate periods", {
  duplicated_periods <- rbind(eps_fixture(), eps_fixture())

  expect_invisible(validate_eps_quarterly(eps_fixture()))
  expect_error(validate_eps_quarterly(duplicated_periods), "duplicate")
})

test_that("quarterly EPS requires the calculated and the published columns", {
  valid <- eps_fixture()

  # Dropping either side would let a published figure be passed off as a
  # calculated one, which is the confusion the contract exists to prevent.
  expect_error(
    validate_eps_quarterly(valid[, setdiff(names(valid), "diluted_eps")]),
    "diluted_eps"
  )
  expect_error(
    validate_eps_quarterly(valid[, setdiff(names(valid), "reported_eps_mxn")]),
    "reported_eps_mxn"
  )
  expect_error(
    validate_eps_quarterly(valid[, setdiff(names(valid), "shares_outstanding")]),
    "shares_outstanding"
  )
})

test_that("manual FactSet snapshots are marked private and validated", {
  annual <- annual_fixture(source = "FactSet manual export", source_url = NA_character_)

  normalized <- normalize_factset_annual(annual)

  expect_identical(normalized$source, "FactSet manual export")
  expect_true(all(is.na(normalized$source_url)))
  expect_identical(names(normalized), FUNDAMENTALS_ANNUAL_COLUMNS)
})

test_that("the committed snapshot matches the contract and the filings it cites", {
  annual <- read_dataset("fundamentals_annual", date_cols = character(0), quiet = TRUE)
  eps <- read_dataset("eps_quarterly", date_cols = "period_end", quiet = TRUE)

  expect_invisible(validate_fundamentals_annual(annual))
  expect_invisible(validate_eps_quarterly(eps))

  # The three fiscal years the assignment asks for.
  expect_setequal(annual$year, c(2023L, 2024L, 2025L))
  # 1Q19 through 4Q25, with no quarter missing.
  expect_equal(nrow(eps), 28L)
  expect_equal(format(min(eps$period_end)), "2019-03-31")
  expect_equal(format(max(eps$period_end)), "2025-12-31")

  # EBITDA is operating income plus depreciation, on every row.
  expect_true(all(abs(
    annual$ebitda_mxn_m - (annual$operating_income_mxn_m + annual$depreciation_mxn_m)
  ) < 0.01))

  # Total financial debt is the sum of its two maturity buckets.
  expect_true(all(abs(
    annual$total_debt_mxn_m - (annual$debt_current_mxn_m + annual$debt_long_term_mxn_m)
  ) < 0.01))

  # The non-controlling interest is real, so the attributable figures are
  # strictly below the consolidated ones.
  expect_true(all(annual$net_income_controlling_mxn_m < annual$net_income_mxn_m))
  expect_true(all(annual$equity_controlling_mxn_m < annual$total_equity_mxn_m))

  # EPS is a calculation, not a transcription: it must reproduce from its own
  # numerator and denominator.
  expect_true(all(abs(
    eps$diluted_eps - eps$net_income_mxn_m * 1e6 / eps$shares_outstanding
  ) < 0.001))

  # And the quarters must add up to the audited year.
  for (year in annual$year) {
    quarterly <- sum(eps$net_income_mxn_m[eps$year == year])
    expect_equal(
      quarterly,
      annual$net_income_mxn_m[annual$year == year],
      tolerance = 1e-4
    )
  }
})
