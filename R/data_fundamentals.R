# Fundamentals data contracts.
#
# FactSet access for the course is workstation-only. Reports therefore prefer
# normalized manual FactSet exports in data/private/ and otherwise use the
# cited public snapshot committed in data/processed/. Both paths share these
# schemas so switching source never changes report code.

# The annual contract carries both the consolidated and the attributable side
# of income and equity. GAP's non-controlling interest -- Vantage's 25.5% of
# Montego Bay and 48.5% of GWTC -- is material enough that a return on equity
# computed on the consolidated pair and one computed on the attributable pair
# differ by more than two percentage points, so the report needs both.
FUNDAMENTALS_ANNUAL_COLUMNS <- c(
  "year", "revenue_reported_mxn_m", "revenue_operating_mxn_m",
  "cost_services_mxn_m",
  "operating_income_mxn_m", "depreciation_mxn_m",
  "net_income_mxn_m", "net_income_controlling_mxn_m", "ebitda_mxn_m",
  "total_assets_mxn_m", "total_equity_mxn_m", "equity_controlling_mxn_m",
  "debt_current_mxn_m", "debt_long_term_mxn_m", "total_debt_mxn_m",
  "cash_mxn_m", "weighted_shares", "reported_eps_mxn",
  "source", "source_url"
)

# The quarterly contract separates what was calculated from what was published.
#
#   diluted_eps                        calculated here: net income over the
#                                      share count the release states
#   reported_eps_mxn / _usd_ads        GAP's own "comprehensive income per
#                                      share / per ADS", which includes other
#                                      comprehensive income and is therefore
#                                      not EPS under IAS 33
#
# Keeping both is the point. The aggregators republish GAP's figure under the
# name "EPS", and a report that cannot show the difference cannot explain it.
EPS_QUARTERLY_COLUMNS <- c(
  "period_end", "year", "quarter", "diluted_eps",
  "net_income_mxn_m", "comprehensive_income_mxn_m",
  "comprehensive_income_controlling_mxn_m", "comprehensive_eps_controlling_mxn",
  "shares_outstanding", "usdmxn", "usdmxn_implied",
  "reported_eps_mxn", "reported_eps_usd_ads",
  "currency", "instrument", "source", "source_url"
)

#' Validate an annual fundamentals snapshot.
#'
#' @param data A data frame following FUNDAMENTALS_ANNUAL_COLUMNS.
#' @return Invisibly, TRUE.
validate_fundamentals_annual <- function(data) {
  missing <- setdiff(FUNDAMENTALS_ANNUAL_COLUMNS, names(data))
  if (length(missing) > 0) {
    stop("Annual fundamentals are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(data$year)) {
    stop("Annual fundamentals contain duplicate years.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a quarterly EPS snapshot.
#'
#' @param data A data frame following EPS_QUARTERLY_COLUMNS.
#' @return Invisibly, TRUE.
validate_eps_quarterly <- function(data) {
  missing <- setdiff(EPS_QUARTERLY_COLUMNS, names(data))
  if (length(missing) > 0) {
    stop("Quarterly EPS is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  key <- paste(data$year, data$quarter, sep = "-")
  if (anyDuplicated(key)) {
    stop("Quarterly EPS contains duplicate periods.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalize a manual FactSet annual export for private storage.
#'
#' @param data A data frame already mapped to the project schema.
#' @return The validated columns in canonical order.
normalize_factset_annual <- function(data) {
  validate_fundamentals_annual(data)
  data[, FUNDAMENTALS_ANNUAL_COLUMNS, drop = FALSE]
}
