# 06 — GAP fundamentals from the filings themselves
#
# Rebuilds both tracked fundamentals datasets from primary documents on SEC
# EDGAR, as listed in config/filings.yml:
#
#   fundamentals_annual.csv   2023-2025, from the FY2025 Form 20-F
#   eps_quarterly.csv         1Q19-4Q25, from 28 Form 6-K earnings releases
#
# Why this exists. Both files used to be typed by hand, which left the leverage
# columns blank for 2023 and 2024 and left the whole EPS series resting on
# third-party aggregators. Everything here is now traceable to a document, and
# the run reconciles the quarterly series against the annual one before it
# writes anything.
#
# On the per-share figures. GAP's releases publish "comprehensive income per
# share", which includes other comprehensive income -- currency translation
# above all -- and divides by shares outstanding rather than by a weighted
# average. That is not earnings per share under IAS 33, and the aggregators
# that republish it as "EPS" are propagating GAP's label, not the standard's.
# This script therefore computes EPS itself, from net income and the share
# count each release states, and keeps GAP's own figure alongside so the two
# can be compared rather than confused.
#
#   Rscript analysis/06_ingest_gap_filings.R
#
# Downloads are cached under data/raw/sec/ and paced for EDGAR's rate limit, so
# a second run costs nothing and touches no network.

source(file.path("R", "utils_io.R"))
source_lib()

# Share counts run to nine figures. Without this they would be written as
# 5.61e+08 and the committed dataset would need a human to interpret it.
options(scipen = 999)

#' Build the EDGAR URL for a filing document.
#'
#' @param accession Accession number with dashes.
#' @param document File name inside the filing.
#' @param cik Central Index Key.
#' @return A single URL.
filing_url <- function(accession, document, cik) {
  paste(
    SEC_ARCHIVES_BASE, cik,
    gsub("-", "", accession, fixed = TRUE), document,
    sep = "/"
  )
}

#' The last calendar day of a fiscal quarter.
#'
#' @param year Fiscal year.
#' @param quarter One of T1 to T4.
#' @return A Date.
quarter_end <- function(year, quarter) {
  ends <- c(T1 = "-03-31", T2 = "-06-30", T3 = "-09-30", T4 = "-12-31")
  as.Date(paste0(year, ends[[quarter]]))
}

#' Reconcile the quarterly series against the audited annual figures.
#'
#' The four quarters of a year must add up to the 20-F. When they do, the
#' quarterly parser has read the right rows; when they do not, something has
#' been misread and the run stops rather than publishing the difference.
#'
#' @param eps The assembled quarterly frame.
#' @param annual The parsed annual frame.
#' @param tolerance Permitted absolute gap, in millions of pesos.
#' @return Invisibly, the comparison table.
reconcile_quarters_to_annual <- function(eps, annual, tolerance = 5) {
  summed <- stats::aggregate(
    net_income_mxn_m ~ year,
    data = eps[eps$year %in% annual$year, ],
    FUN = sum
  )
  names(summed)[2] <- "quarterly_sum"

  comparison <- merge(
    summed,
    annual[, c("year", "net_income_mxn_m")],
    by = "year"
  )
  names(comparison)[3] <- "annual_20f"
  comparison$difference <- comparison$quarterly_sum - comparison$annual_20f

  message("\nReconciliation, net income (MXN million):")
  print(comparison, row.names = FALSE)

  failed <- abs(comparison$difference) > tolerance
  if (any(failed)) {
    stop(
      "Quarterly net income does not reconcile to the 20-F for: ",
      paste(comparison$year[failed], collapse = ", "), ".\n",
      "  Investigate before trusting either series.",
      call. = FALSE
    )
  }

  invisible(comparison)
}

#' Report where GAP's own per-share figure disagrees with its inputs.
#'
#' GAP's published figure should equal comprehensive income over the share
#' count it names in the same footnote. Where it does not, the release itself
#' contains a typographical error, and saying so is more useful than silently
#' carrying it forward. Nothing this project reports depends on the figure.
#'
#' @param eps The assembled quarterly frame.
#' @return Invisibly, the rows that disagree.
report_reported_eps_anomalies <- function(eps) {
  implied <- round(eps$comprehensive_income_mxn_m * 1e6 / eps$shares_outstanding, 4)
  dropped <- is.na(eps$reported_eps_mxn)
  divergent <- !dropped & abs(implied - eps$reported_eps_mxn) > 0.02

  if (any(dropped)) {
    message(
      "\nDropped as unreadable -- GAP's published per-share figure is not ",
      "recoverable from the document:"
    )
    print(
      data.frame(
        period = paste(eps$year, eps$quarter)[dropped],
        implied_by_footnote = implied[dropped]
      ),
      row.names = FALSE
    )
  }

  if (any(divergent)) {
    message("\nKept, but GAP's published figure misses its own footnote:")
    print(
      data.frame(
        period = paste(eps$year, eps$quarter)[divergent],
        published = eps$reported_eps_mxn[divergent],
        implied_by_footnote = implied[divergent]
      ),
      row.names = FALSE
    )
  }

  if (any(dropped) || any(divergent)) {
    message("  The calculated EPS series does not depend on any of these.")
  }

  invisible(eps[dropped | divergent, , drop = FALSE])
}

#' Report where the translation rate a release used is not the one it quotes.
#'
#' Relevant only to the ADS reconciliation, and to nothing computed in pesos.
#'
#' @param eps The assembled quarterly frame.
#' @return Invisibly, the divergent rows.
report_fx_divergence <- function(eps) {
  divergent <- !is.na(eps$usdmxn_implied) &
    abs(eps$usdmxn_implied - eps$usdmxn) > 0.01 * eps$usdmxn

  if (any(divergent)) {
    message("\nGAP's per-ADS figure was translated at a rate other than the one it quotes:")
    print(
      data.frame(
        period = paste(eps$year, eps$quarter)[divergent],
        quoted = eps$usdmxn[divergent],
        implied_by_published_ads = eps$usdmxn_implied[divergent]
      ),
      row.names = FALSE
    )
    message("  Peso figures are unaffected; only the per-ADS series uses a rate.")
  }

  invisible(eps[divergent, , drop = FALSE])
}

main <- function() {
  config <- filings_config()
  cik <- config$cik

  message("Annual fundamentals from ", config$annual$label, " ...")
  annual_spec <- config$annual
  annual <- parse_20f_annual(annual_spec)
  message("  ", nrow(annual), " fiscal years: ", paste(annual$year, collapse = ", "))

  message("\nQuarterly releases (", length(config$quarterly), ") ...")
  quarterly <- lapply(config$quarterly, function(spec) {
    spec$url <- filing_url(spec$accession, spec$document, cik)
    parse_gap_release(spec)
  })
  eps <- do.call(rbind, quarterly)

  eps$period_end <- as.Date(vapply(
    seq_len(nrow(eps)),
    function(i) format(quarter_end(eps$year[i], eps$quarter[i])),
    character(1)
  ))

  # EPS as IAS 33 defines it: earnings over shares. GAP does not split net
  # income between controlling and non-controlling interests quarterly, so the
  # numerator is consolidated net income and the report says so.
  eps$diluted_eps <- round(eps$net_income_mxn_m * 1e6 / eps$shares_outstanding, 4)

  # The attributable measure GAP does publish quarterly, on the same shares.
  eps$comprehensive_eps_controlling_mxn <- round(
    eps$comprehensive_income_controlling_mxn_m * 1e6 / eps$shares_outstanding, 4
  )

  eps$currency <- "MXN"
  eps$instrument <- "GAPB"

  eps <- eps[order(eps$period_end), ]
  rownames(eps) <- NULL

  reconcile_quarters_to_annual(eps, annual)
  report_reported_eps_anomalies(eps)
  report_fx_divergence(eps)

  validate_fundamentals_annual(annual)
  validate_eps_quarterly(eps)

  write_processed(annual[, FUNDAMENTALS_ANNUAL_COLUMNS], "fundamentals_annual")
  write_processed(eps[, EPS_QUARTERLY_COLUMNS], "eps_quarterly")

  message(
    "\nDone. ", nrow(eps), " quarters, ",
    format(min(eps$period_end)), " to ", format(max(eps$period_end)), "."
  )
  invisible(list(annual = annual, eps = eps))
}

main()
