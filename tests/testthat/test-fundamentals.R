test_that("annual fundamentals require the report contract", {
  valid <- data.frame(
    year = 2025L,
    revenue_reported_mxn_m = 1,
    revenue_operating_mxn_m = 1,
    cost_services_mxn_m = 1,
    operating_income_mxn_m = 1,
    net_income_mxn_m = 1,
    ebitda_mxn_m = 1,
    total_assets_mxn_m = 1,
    total_equity_mxn_m = 1,
    total_debt_mxn_m = 1,
    cash_mxn_m = 1,
    source = "GAP",
    source_url = "https://example.com"
  )

  expect_invisible(validate_fundamentals_annual(valid))
  expect_error(
    validate_fundamentals_annual(valid[, setdiff(names(valid), "cost_services_mxn_m")]),
    "cost_services_mxn_m"
  )
})

test_that("quarterly EPS rejects duplicate periods", {
  eps <- data.frame(
    period_end = as.Date(c("2025-03-31", "2025-03-31")),
    year = c(2025L, 2025L),
    quarter = c("T1", "T1"),
    diluted_eps = c(1, 1),
    currency = c("USD", "USD"),
    instrument = c("PAC ADS", "PAC ADS"),
    source = c("public", "public"),
    source_url = c("https://example.com", "https://example.com")
  )

  expect_error(validate_eps_quarterly(eps), "duplicate")
})

test_that("manual FactSet snapshots are marked private and validated", {
  annual <- data.frame(
    year = 2025L,
    revenue_reported_mxn_m = 1,
    revenue_operating_mxn_m = 1,
    cost_services_mxn_m = 1,
    operating_income_mxn_m = 1,
    net_income_mxn_m = 1,
    ebitda_mxn_m = 1,
    total_assets_mxn_m = 1,
    total_equity_mxn_m = 1,
    total_debt_mxn_m = 1,
    cash_mxn_m = 1,
    source = "FactSet manual export",
    source_url = NA_character_
  )

  normalized <- normalize_factset_annual(annual)

  expect_identical(normalized$source, "FactSet manual export")
  expect_true(all(is.na(normalized$source_url)))
})
