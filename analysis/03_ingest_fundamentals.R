# 03 — Company fundamentals ingestion (FactSet)
#
# The one step in the pipeline that requires credentials, and the one whose
# output is never committed. Extracts land in data/raw/factset/, which is
# excluded from version control, because FactSet content is cited but not
# redistributed.
#
# Without an entitled key this script stops with a named error and changes
# nothing. Every other stage of the pipeline continues to work.
#
#   Rscript analysis/03_ingest_fundamentals.R

source(file.path("R", "utils_io.R"))
source_lib()

library(httr2)

main <- function() {
  load_env()

  print(env_paths(), row.names = FALSE)

  status <- env_status(c("FACTSET_USERNAME_SERIAL", "FACTSET_API_KEY"))
  print(status, row.names = FALSE)

  if (!all(status$set)) {
    stop(
      "FactSet credentials are not configured.\n",
      "  Copy .env.example to .env and fill in FACTSET_USERNAME_SERIAL and ",
      "FACTSET_API_KEY.\n",
      "  Every other step of the pipeline runs without them.",
      call. = FALSE
    )
  }

  # Settle entitlement before pulling anything substantial. A Workstation
  # licence does not imply API access, and finding that out on a large request
  # wastes both time and quota.
  message("Checking entitlement ...")
  factset_ping()

  meta <- asset_meta()
  message("Fetching quarterly fundamentals for ", meta$name, " (", meta$factset_id, ") ...")

  fundamentals <- fetch_factset_fundamentals(
    ids = meta$factset_id,
    start = "2019-01-01",
    end = params()$sample$end,
    periodicity = "QTR"
  )

  message(
    "Wrote ", nrow(fundamentals), " observations to data/raw/factset/fundamentals.csv ",
    "(untracked, by design)."
  )
  print(utils::head(fundamentals, 10), row.names = FALSE)

  invisible(fundamentals)
}

main()
