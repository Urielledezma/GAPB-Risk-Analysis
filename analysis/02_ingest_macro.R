# 02 — Macroeconomic data ingestion
#
# Writes data/processed/gdp_mx.csv from the World Bank, which needs no
# credentials. Banxico and INEGI series are optional enrichments: without a
# token they are skipped with a message and the pipeline continues.
#
#   Rscript analysis/02_ingest_macro.R

source(file.path("R", "utils_io.R"))
source_lib()

library(httr2)

fetch_optional <- function(label, fn) {
  tryCatch(
    fn(),
    error = function(e) {
      message("Skipping ", label, ": ", conditionMessage(e))
      NULL
    }
  )
}

main <- function() {
  load_env()

  # --- Required: annual GDP growth, no credentials --------------------------
  message("Fetching annual GDP growth from the World Bank ...")
  gdp <- fetch_gdp_worldbank()
  write_processed(gdp, "gdp_mx")
  message(
    "  ", nrow(gdp), " years, ", min(gdp$year), " to ", max(gdp$year)
  )

  # --- Optional: Banxico reference rate and FX ------------------------------
  macro <- assets_config()$macro
  banxico <- list()
  for (key in c("reference_rate", "fx_usdmxn")) {
    spec <- macro[[key]]
    series <- fetch_optional(
      paste0("Banxico ", spec$series, " (", spec$label, ")"),
      function() fetch_banxico_series(spec$series)
    )
    if (!is.null(series)) {
      series$label <- spec$label
      banxico[[key]] <- series
      message("  ", spec$series, ": ", nrow(series), " observations")
    }
  }
  if (length(banxico) > 0) {
    write_processed(do.call(rbind, banxico), "rates_mx")
  }

  # --- Optional: INEGI quarterly GDP ----------------------------------------
  spec <- macro$gdp_quarterly
  quarterly <- fetch_optional(
    "INEGI quarterly GDP",
    function() {
      fetch_inegi_indicator(
        spec$indicator,
        geography = spec$geography %||% "00",
        bank = spec$bank %||% "BIE"
      )
    }
  )
  if (!is.null(quarterly)) {
    write_processed(quarterly, "gdp_mx_quarterly")
    message("  ", nrow(quarterly), " quarters")
  }

  invisible(TRUE)
}

main()
