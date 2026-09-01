# 03 — Manual FactSet fundamentals ingestion
#
# ITESO's FactSet licence includes workstation access but no API entitlement.
# Export the two report tables from FactSet, map them to the documented schemas
# below and place them in data/raw/factset/manual/. This script validates and
# normalizes them into data/private/, which remains untracked. Reports prefer
# those private files and otherwise use the cited public snapshot.
#
# Required files:
#   fundamentals_annual.csv — FUNDAMENTALS_ANNUAL_COLUMNS
#   eps_quarterly.csv        — EPS_QUARTERLY_COLUMNS
#
#   Rscript analysis/03_ingest_fundamentals.R

source(file.path("R", "utils_io.R"))
source_lib()

main <- function() {
  input_dir <- proj_path("data", "raw", "factset", "manual")
  annual_path <- file.path(input_dir, "fundamentals_annual.csv")
  eps_path <- file.path(input_dir, "eps_quarterly.csv")

  missing <- c(annual_path, eps_path)[!file.exists(c(annual_path, eps_path))]
  if (length(missing) > 0) {
    stop(
      "Missing manual FactSet export(s):\n  ",
      paste(missing, collapse = "\n  "),
      "\nUse the column contracts in R/data_fundamentals.R.",
      call. = FALSE
    )
  }

  annual <- utils::read.csv(annual_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  eps <- utils::read.csv(eps_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  eps$period_end <- as.Date(eps$period_end)

  annual <- normalize_manual_factset_annual(annual)
  validate_eps_quarterly(eps)

  write_private(annual, "fundamentals_annual.csv")
  write_private(eps[, EPS_QUARTERLY_COLUMNS], "eps_quarterly.csv")
  message("Manual FactSet fundamentals are ready for report rendering.")
  invisible(list(annual = annual, eps = eps))
}

main()
