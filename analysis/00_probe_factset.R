# 00 — FactSet probe
#
# Run this once, before anything else, on a machine with an entitled key. It
# settles in three cheap calls what would otherwise be guessed for the whole
# project:
#
#   1. Do the credentials authenticate at all (401 versus 403)?
#   2. Is the account entitled to Prices, and to Fundamentals?
#   3. What field names does the response actually use, and is the identifier
#      convention in config/assets.yml correct for BMV listings?
#
# Nothing is written to a dataset. The output is a printed report of the raw
# response structure, which is what the parsers in R/data_factset.R are built
# against.
#
#   Rscript analysis/00_probe_factset.R

source(file.path("R", "utils_io.R"))
source_lib()

library(httr2)

describe_row <- function(row, label) {
  cat("\n", label, " — fields returned:\n", sep = "")
  for (name in names(row)) {
    value <- row[[name]]
    rendered <- if (is.null(value)) {
      "NULL"
    } else {
      paste(utils::head(as.character(unlist(value)), 2), collapse = ", ")
    }
    cat(sprintf("  %-22s %-10s %s\n", name, class(value)[1], rendered))
  }
}

probe_endpoint <- function(label, path, query) {
  cat("\n", strrep("-", 70), "\n", label, "\n", strrep("-", 70), "\n", sep = "")
  result <- tryCatch(
    factset_get(path, query),
    error = function(e) {
      cat("FAILED\n  ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )
  if (is.null(result)) {
    return(invisible(NULL))
  }

  observations <- result$data
  cat("OK — ", length(observations), " observation(s) returned.\n", sep = "")
  if (length(observations) > 0) {
    describe_row(observations[[1]], "First observation")
  }
  invisible(result)
}

main <- function() {
  load_env()

  cat("Credential files searched:\n")
  print(env_paths(), row.names = FALSE)

  cat("\nCredential status (names and lengths only):\n")
  print(env_status(c("FACTSET_USERNAME_SERIAL", "FACTSET_API_KEY")), row.names = FALSE)

  if (!all(env_status(c("FACTSET_USERNAME_SERIAL", "FACTSET_API_KEY"))$set)) {
    stop(
      "FactSet credentials are not configured. Copy .env.example to .env first.",
      call. = FALSE
    )
  }

  meta <- asset_meta()
  cat("\nProbing with identifier: ", meta$factset_id, " (", meta$ticker, ")\n", sep = "")

  probe_endpoint(
    "Prices — one week, daily",
    c("factset-prices", "v1", "prices"),
    list(
      ids = meta$factset_id,
      startDate = format(Sys.Date() - 10, "%Y-%m-%d"),
      endDate = format(Sys.Date(), "%Y-%m-%d"),
      frequency = "D",
      adjust = "SPLIT_SPINOFF"
    )
  )

  probe_endpoint(
    "Fundamentals — one metric, one year",
    c("factset-fundamentals", "v2", "fundamentals"),
    list(
      ids = meta$factset_id,
      metrics = "FF_EPS_DIL",
      periodicity = "QTR",
      fiscalPeriodStart = "2024-01-01",
      fiscalPeriodEnd = "2024-12-31"
    )
  )

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("What to do with this output:\n")
  cat("  - Fields listed above but missing from FACTSET_PRICE_FIELDS in\n")
  cat("    R/data_factset.R should be added there, and the guesses narrowed.\n")
  cat("  - An empty Prices response means the identifier convention is wrong.\n")
  cat("    Try GAPB-MX, GAP.B-MX or the FactSet permanent id.\n")
  cat("  - A 403 on one endpoint and not the other means partial entitlement;\n")
  cat("    the pipeline is built to work that way.\n")
  cat("  - Confirm whether the price series is dividend-adjusted. If it is not,\n")
  cat("    the total-return treatment in R/data_factset.R needs revisiting.\n")

  invisible(TRUE)
}

main()
