# GAP filings on SEC EDGAR.
#
# The fundamentals in this project used to be a hand-typed snapshot. They are
# now read from the filings themselves, because a figure nobody can trace back
# to a document is a figure nobody can defend in a viva.
#
# Two document families, two parsers:
#
#   20-F      The annual report. EDGAR renders each financial statement as a
#             small R<n>.htm table, so the balance sheet and the income
#             statement arrive already tabulated and carry three fiscal years
#             at once. That is the source for everything annual.
#   6-K       The quarterly earnings release. There is no quarterly XBRL for a
#             foreign private issuer, so the figures live in the press-release
#             tables. Every release states its own share count in a footnote,
#             which is what makes a per-share calculation possible at all.
#
# Nothing here is called from a report. analysis/06_ingest_gap_filings.R drives
# it and writes the result to data/processed/.

SEC_ARCHIVES_BASE <- "https://www.sec.gov/Archives/edgar/data"

# EDGAR rejects anonymous traffic and throttles above ten requests a second.
# One request every 0.4 s leaves a wide margin and still finishes the whole
# manifest in under a minute.
SEC_REQUEST_PAUSE <- 0.4

# One ADS represents ten Series B shares. Fixed since the 2006 listing, and not
# a split: it is the ratio of the depositary receipt to the underlying share.
ADS_SHARE_RATIO <- 10

#' The User-Agent EDGAR requires.
#'
#' SEC's access policy asks for a declared identity on every automated request.
#' Set SEC_USER_AGENT in .env to override the fallback.
#'
#' @return A single string.
sec_user_agent <- function() {
  configured <- Sys.getenv("SEC_USER_AGENT", unset = "")
  if (nzchar(configured)) {
    return(configured)
  }
  "GAPB-Risk-Analysis academic project (urielledezma7@gmail.com)"
}

#' Download one document from EDGAR, through a local cache.
#'
#' The cache is what keeps a re-run from hitting EDGAR again: the filings are
#' immutable once accepted, so a cached copy is not a staleness risk.
#'
#' @param accession Accession number, with or without dashes.
#' @param document File name inside the filing.
#' @param cik Central Index Key.
#' @param refresh Ignore the cache and fetch again.
#' @return The document as a single string.
fetch_sec_document <- function(accession, document, cik = "1347557", refresh = FALSE) {
  bare <- gsub("-", "", accession, fixed = TRUE)
  cache_path <- proj_path("data", "raw", "sec", bare, document)

  if (!refresh && file.exists(cache_path)) {
    return(paste(readLines(cache_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }

  url <- paste(SEC_ARCHIVES_BASE, cik, bare, document, sep = "/")
  message("  fetching ", accession, "/", document)

  body <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(sec_user_agent()) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(120) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) {
      stop(
        "EDGAR request failed for ", accession, "/", document, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  ensure_dir(dirname(cache_path))
  writeLines(body, cache_path, useBytes = TRUE)
  Sys.sleep(SEC_REQUEST_PAUSE)
  body
}

#' Every table row in an HTML document, as a list of character vectors.
#'
#' Cells are squished and empty ones dropped, so a row arrives as the label
#' followed by whatever the filer split its numbers into.
#'
#' @param html A document as a single string.
#' @return A list of character vectors, in document order.
sec_table_rows <- function(html) {
  document <- xml2::read_html(html)
  rows <- xml2::xml_find_all(document, "//tr")

  lapply(rows, function(row) {
    cells <- xml2::xml_text(xml2::xml_find_all(row, "./td | ./th"))
    cells <- trimws(gsub("[[:space:] ]+", " ", cells))
    cells[nzchar(cells)]
  })
}

#' Normalize a row label so its wording variants collapse to one key.
#'
#' Across seven years GAP writes "Net income", "Net income (loss)" and
#' "Net (loss) income" for the same line. Stripping the bracketed polarity is
#' what lets one pattern match all three.
#'
#' @param label A raw label string.
#' @return A lowercase, squished label.
normalize_statement_label <- function(label) {
  label <- gsub("\\((loss|income|profit)\\)", " ", label, ignore.case = TRUE)
  # Note references move between filings; the line they annotate does not.
  label <- gsub("\\([^)]*note[^)]*\\)", " ", label, ignore.case = TRUE)
  label <- gsub("[‘’ʼ]", "'", label)
  label <- gsub("[–—]", "-", label)
  label <- gsub("[[:space:] ]+", " ", label)
  tolower(trimws(label))
}

#' Pull the numbers out of a table row.
#'
#' Filers split a single negative number across three cells -- "(307,548", ")"
#' and the percentage that follows -- so the cells are rejoined before any
#' number is read. Percentage columns are dropped: they are the filer's own
#' change calculation, never a figure this project reports.
#'
#' @param cells A character vector of cells, label included.
#' @return A numeric vector, in column order.
statement_row_values <- function(cells) {
  if (length(cells) < 2) {
    return(numeric(0))
  }

  joined <- paste(cells[-1], collapse = " ")
  joined <- gsub("\\( ", "(", joined)
  joined <- gsub(" \\)", ")", joined)
  joined <- gsub(" ?%", "%", joined)

  matches <- regmatches(
    joined,
    gregexpr("\\(?-?[0-9][0-9,]*(?:\\.[0-9]+)?%?\\)?", joined)
  )[[1]]

  matches <- matches[!grepl("%", matches, fixed = TRUE)]
  if (length(matches) == 0) {
    return(numeric(0))
  }

  negative <- grepl("^\\(", matches)
  numbers <- gsub("[(),]", "", matches)
  numbers <- suppressWarnings(as.numeric(numbers))
  numbers[negative] <- -abs(numbers[negative])
  numbers[!is.na(numbers)]
}

#' The first row whose label matches, with its values.
#'
#' First rather than last on purpose: GAP prints the quarter before the
#' cumulative year in every release, so the first match is the quarterly figure
#' and the second is year to date.
#'
#' @param rows Output of sec_table_rows().
#' @param label A normalized label to match exactly.
#' @param min_values Minimum number of numeric columns the row must carry.
#' @return A numeric vector, or NULL when no row matches.
find_statement_row <- function(rows, label, min_values = 2) {
  for (cells in rows) {
    if (length(cells) == 0) {
      next
    }
    if (!identical(normalize_statement_label(cells[1]), label)) {
      next
    }
    values <- statement_row_values(cells)
    if (length(values) >= min_values) {
      return(values)
    }
  }
  NULL
}

#' The same, but raising instead of returning NULL.
#'
#' A missing line means the parser has drifted from the document, which is a
#' condition to stop on rather than to fill with NA.
#'
#' @param rows Output of sec_table_rows().
#' @param label A normalized label to match exactly.
#' @param context Identifier used in the error message.
#' @param min_values Minimum number of numeric columns the row must carry.
#' @return A numeric vector.
require_statement_row <- function(rows, label, context, min_values = 2) {
  values <- find_statement_row(rows, label, min_values = min_values)
  if (is.null(values)) {
    stop(
      "Could not find the line '", label, "' in ", context, ".\n",
      "  The filing layout has changed; the parser needs revisiting.",
      call. = FALSE
    )
  }
  values
}

# ---------------------------------------------------------------------------
# 20-F: the annual statements
# ---------------------------------------------------------------------------

#' Annual fundamentals from one 20-F.
#'
#' EDGAR renders each statement of a filing as its own small table, so the
#' balance sheet arrives as R2 and the income statement as R3, each already
#' carrying three fiscal years. One filing therefore covers 2023 to 2025 on a
#' single accounting basis -- which matters more than it sounds, because
#' stitching three filings together would mix pre- and post-restatement figures.
#'
#' Amounts are reported in thousands of pesos and returned in millions, the
#' unit every table in the report uses.
#'
#' @param spec A list with accession, balance_sheet, income_statement and url.
#' @param refresh Ignore the download cache.
#' @return A data frame following FUNDAMENTALS_ANNUAL_COLUMNS.
parse_20f_annual <- function(spec, refresh = FALSE) {
  context <- paste0("20-F ", spec$accession)

  balance <- sec_table_rows(
    fetch_sec_document(spec$accession, spec$balance_sheet, refresh = refresh)
  )
  income <- sec_table_rows(
    fetch_sec_document(spec$accession, spec$income_statement, refresh = refresh)
  )

  line <- function(rows, label, min_values = 3) {
    require_statement_row(rows, label, context, min_values = min_values)[1:3]
  }

  # Balance sheet. GAP splits financial debt across one current and two
  # non-current lines; leases and payables are not debt and stay out.
  cash <- line(balance, "cash and cash equivalents")
  debt_current <- line(balance, "bank loans, debt securities and current portion of debt")
  debt_bank_lt <- line(balance, "long-term borrowings")
  debt_securities_lt <- line(balance, "debt securities")
  assets <- line(balance, "total")
  equity <- line(balance, "total stockholders' equity")
  equity_controlling <- line(balance, "total equity attributable to controlling interest")

  # Income statement.
  aeronautical <- line(income, "aeronautical services")
  non_aeronautical <- line(income, "non-aeronautical services")
  revenue_reported <- line(income, "revenues")
  operating_income <- line(income, "income from operations")
  depreciation <- line(income, "depreciation and amortization")
  net_income <- line(income, "net profit for the year")
  net_income_controlling <- line(income, "controlling interest")
  weighted_shares <- line(income, "weighted average number of common shares outstanding")
  reported_eps <- line(income, "diluted earnings per share")

  # The cost of running the airports, before the concession tax, the technical
  # assistance fee, depreciation and the IFRIC 12 construction cost. This is
  # the aggregate the gross-margin proxy divides by.
  cost_services <- Reduce(`+`, list(
    line(income, "employee cost"),
    line(income, "maintenance"),
    line(income, "safety, security and insurance"),
    line(income, "utilities"),
    line(income, "expected credit loss of the year"),
    line(income, "others operation expenses")
  ))

  # Statements read newest first; the project reads oldest first.
  order <- rev(seq_len(3))
  years <- rev(spec$years)

  to_millions <- function(x) round(x[order] / 1000, 3)

  annual <- data.frame(
    year = as.integer(years),
    revenue_reported_mxn_m = to_millions(revenue_reported),
    revenue_operating_mxn_m = to_millions(aeronautical + non_aeronautical),
    cost_services_mxn_m = to_millions(cost_services),
    operating_income_mxn_m = to_millions(operating_income),
    depreciation_mxn_m = to_millions(depreciation),
    net_income_mxn_m = to_millions(net_income),
    net_income_controlling_mxn_m = to_millions(net_income_controlling),
    ebitda_mxn_m = to_millions(operating_income + depreciation),
    total_assets_mxn_m = to_millions(assets),
    total_equity_mxn_m = to_millions(equity),
    equity_controlling_mxn_m = to_millions(equity_controlling),
    debt_current_mxn_m = to_millions(debt_current),
    debt_long_term_mxn_m = to_millions(debt_bank_lt + debt_securities_lt),
    total_debt_mxn_m = to_millions(debt_current + debt_bank_lt + debt_securities_lt),
    cash_mxn_m = to_millions(cash),
    weighted_shares = weighted_shares[order],
    reported_eps_mxn = reported_eps[order],
    source = spec$label,
    source_url = spec$url,
    stringsAsFactors = FALSE
  )

  validate_20f_annual(annual)
  annual
}

#' Internal consistency checks on a parsed 20-F.
#'
#' Each one compares two numbers the filing states independently. A parser that
#' has grabbed the wrong row almost always fails at least one of them, which is
#' the point: a silent mis-parse is the failure mode worth engineering against.
#'
#' @param annual A parsed annual frame.
#' @return Invisibly, TRUE.
validate_20f_annual <- function(annual) {
  check <- function(condition, message) {
    if (!all(condition)) {
      stop("20-F consistency check failed: ", message, call. = FALSE)
    }
  }

  check(
    abs(annual$revenue_reported_mxn_m - annual$revenue_operating_mxn_m) > 0,
    "reported revenue does not exceed operating revenue; IFRIC 12 line missing"
  )
  check(
    annual$total_equity_mxn_m > annual$equity_controlling_mxn_m,
    "total equity is not above equity attributable to the controlling interest"
  )
  check(
    annual$net_income_mxn_m > annual$net_income_controlling_mxn_m,
    "net income is not above the share attributable to the controlling interest"
  )
  check(
    abs(annual$ebitda_mxn_m -
          (annual$operating_income_mxn_m + annual$depreciation_mxn_m)) < 0.01,
    "EBITDA does not reconcile to operating income plus depreciation"
  )

  implied_eps <- annual$net_income_controlling_mxn_m * 1e6 / annual$weighted_shares
  check(
    abs(implied_eps - annual$reported_eps_mxn) < 0.01,
    "reported EPS does not reconcile to attributable profit over weighted shares"
  )

  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 6-K: the quarterly earnings releases
# ---------------------------------------------------------------------------

#' Figures for one quarter from one earnings release.
#'
#' Each release prints the quarter beside the same quarter a year earlier, so
#' `column` decides which of the two is wanted. Almost every quarter is read
#' from its own release; the exception is documented in config/filings.yml.
#'
#' The share count is not derived. GAP states it in a footnote of every release
#' -- "calculated based on N outstanding shares" -- and that footnote is the
#' denominator, so no assumption about buybacks has to be made anywhere.
#'
#' @param spec A list with year, quarter, accession, document, column and url.
#' @param refresh Ignore the download cache.
#' @return A one-row data frame.
parse_gap_release <- function(spec, refresh = FALSE) {
  context <- paste0(spec$year, " ", spec$quarter, " release ", spec$accession)
  html <- fetch_sec_document(spec$accession, spec$document, refresh = refresh)
  rows <- sec_table_rows(html)

  index <- if (identical(spec$column, "prior")) 1L else 2L
  pick <- function(label) {
    require_statement_row(rows, label, context)[index]
  }

  net_income <- pick("net income")
  comprehensive <- pick("comprehensive income")
  comprehensive_controlling <- pick("comprehensive income attributable to controlling interest")
  reported_per_share <- pick("comprehensive income per share (pesos)")
  reported_per_ads <- pick("comprehensive income per ads (us dollars)")

  shares <- extract_release_shares(html, context)
  usdmxn <- extract_release_fx(html, context)

  # A published per-share figure that misses its own footnote by more than a
  # few percent is not a figure. Two 2020 releases print one -- 2Q20 truncates
  # the cell mid-number, 3Q20 prints a bare zero -- and carrying either forward
  # would put a wrong number in a tracked dataset. They are dropped rather than
  # repaired, because the correct value is not recoverable from the document.
  implied_per_share <- comprehensive / shares * 1000
  unusable <- !is.finite(reported_per_share) ||
    abs(reported_per_share - implied_per_share) > 0.05 * abs(implied_per_share)
  if (unusable) {
    reported_per_share <- NA_real_
    reported_per_ads <- NA_real_
  }

  # One ADS is ten Series B shares, so the two published per-share figures and
  # a translation rate are three views of one number. Both rates are recorded
  # because they do not always agree. `usdmxn` is the closing noon buying rate
  # the release names, which is the economically correct one; `usdmxn_implied`
  # is the rate GAP's own published pair requires. In several quarters -- 2Q24
  # and 3Q24 among them -- GAP translated at the previous quarter's rate and
  # left the footnote unchanged, so its published per-ADS figure is stale. That
  # is one reason the peso series, not the ADS one, is the series this project
  # reports.
  implied_fx <- NA_real_
  if (!is.na(reported_per_ads) && abs(reported_per_ads) > 1e-8) {
    implied_fx <- round(reported_per_share * ADS_SHARE_RATIO / reported_per_ads, 4)
  }

  data.frame(
    year = as.integer(spec$year),
    quarter = spec$quarter,
    net_income_mxn_m = round(net_income / 1000, 3),
    comprehensive_income_mxn_m = round(comprehensive / 1000, 3),
    comprehensive_income_controlling_mxn_m = round(comprehensive_controlling / 1000, 3),
    shares_outstanding = shares,
    usdmxn = usdmxn,
    usdmxn_implied = implied_fx,
    reported_eps_mxn = reported_per_share,
    reported_eps_usd_ads = reported_per_ads,
    source = spec$label,
    source_url = spec$url,
    stringsAsFactors = FALSE
  )
}

#' The share count a release states for its own per-share figures.
#'
#' The first occurrence is the quarterly footnote; a second one, when present,
#' belongs to the cumulative table below it and uses a different average.
#'
#' @param html A release as a single string.
#' @param context Identifier used in the error message.
#' @return A single numeric.
extract_release_shares <- function(html, context) {
  text <- gsub("<[^>]*>", " ", html)
  text <- gsub("[[:space:] ]+", " ", text)
  found <- regmatches(
    text,
    gregexpr("based on ([0-9][0-9,]*) (?:outstanding |issued )?shares", text)
  )[[1]]

  if (length(found) == 0) {
    stop(
      "No share-count footnote in ", context, ".\n",
      "  Every GAP release states its own denominator; the parser must find it.",
      call. = FALSE
    )
  }

  as.numeric(gsub("[^0-9]", "", found[1]))
}

#' The peso/dollar rate a release used for its per-ADS figures.
#'
#' A release quotes several rates -- the average for the quarter, the average
#' for the year, the closing rate -- and only one of them translated the
#' per-ADS figure. The pattern therefore anchors on the translation sentence
#' itself rather than on the first rate that appears.
#'
#' @param html A release as a single string.
#' @param context Identifier used in the error message.
#' @return A single numeric.
extract_release_fx <- function(html, context) {
  text <- gsub("<[^>]*>", " ", html)
  text <- gsub("[[:space:] ]+", " ", text)
  # Two wordings across the period -- "at a rate of" before 2025, "using an
  # exchange rate of" after -- and a third sentence quoting the average rate
  # used to consolidate Jamaica, which must not be picked up. Anchoring on
  # "converted from pesos" is what separates the translation rate from it.
  pattern <- paste0(
    "converted from pesos.{0,60}?rate of ",
    "Ps\\.? ?([0-9]+\\.[0-9]+) per U\\.S\\. dollar"
  )
  found <- regmatches(text, gregexpr(pattern, text))[[1]]

  if (length(found) == 0) {
    stop("No exchange-rate footnote in ", context, ".", call. = FALSE)
  }

  # "Ps." and "U.S." both contribute dots, so the rate has to be captured
  # rather than filtered out of the surrounding sentence.
  as.numeric(sub(paste0("^.*", pattern, ".*$"), "\\1", found[1]))
}
