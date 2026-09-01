# Fundamentals data contracts.
#
# FactSet access for the course is workstation-only. Reports therefore prefer
# normalized manual FactSet exports in data/private/ and otherwise use the
# cited public snapshot committed in data/processed/. Both paths share these
# schemas so switching source never changes report code.

FUNDAMENTALS_ANNUAL_COLUMNS <- c(
  "year", "revenue_reported_mxn_m", "revenue_operating_mxn_m",
  "cost_services_mxn_m",
  "operating_income_mxn_m", "net_income_mxn_m", "ebitda_mxn_m",
  "total_assets_mxn_m", "total_equity_mxn_m", "total_debt_mxn_m",
  "cash_mxn_m", "source", "source_url"
)

EPS_QUARTERLY_COLUMNS <- c(
  "period_end", "year", "quarter", "diluted_eps", "currency", "instrument",
  "source", "source_url"
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
normalize_manual_factset_annual <- function(data) {
  validate_fundamentals_annual(data)
  data[, FUNDAMENTALS_ANNUAL_COLUMNS, drop = FALSE]
}
