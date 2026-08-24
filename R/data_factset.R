# FactSet REST client.
#
# FactSet publishes no maintained R SDK -- the only R client in its GitHub
# organisation was archived in 2022 -- so this is a thin httr2 wrapper over the
# REST endpoints, using HTTP Basic authentication with the username-serial as
# the user and the API key as the password.
#
# Everything this file downloads lands in data/raw/factset/, which is excluded
# from version control. FactSet content is not redistributed: reports cite it as
# a source, in the way any research note cites a data vendor, and the extracts
# stay on the analyst's machine.
#
# This is also the only part of the pipeline that can be blocked by an
# entitlement rather than a bug. An account with a Workstation licence does not
# necessarily carry API entitlements, and an entitled account does not
# necessarily cover every endpoint. factset_ping() exists to settle that in one
# cheap call before anything is built on top of it.

FACTSET_BASE <- "https://api.factset.com/content"

# Fundamental metrics pulled for the issuer profile. FactSet metric codes are
# provisional until confirmed against /v2/metrics on an entitled account.
FACTSET_FUNDAMENTAL_METRICS <- c(
  "FF_SALES",        # Revenue
  "FF_GROSS_INC",    # Gross profit
  "FF_OPER_INC",     # Operating income
  "FF_NET_INC",      # Net income
  "FF_EBITDA_OPER",  # EBITDA
  "FF_EPS_DIL",      # Diluted earnings per share
  "FF_ROE",          # Return on equity
  "FF_ROA",          # Return on assets
  "FF_DEBT",         # Total debt
  "FF_EQ_TOT",       # Total equity
  "FF_ASSETS"        # Total assets
)

#' Build an authenticated FactSet request.
#'
#' @param path Path components below the content root.
#' @return An httr2 request.
factset_request <- function(path) {
  user <- require_env(
    "FACTSET_USERNAME_SERIAL",
    "Create one at https://developer.factset.com -> Create API Key"
  )
  key <- require_env(
    "FACTSET_API_KEY",
    "The generated secret shown once when the key is created."
  )

  request <- do.call(
    httr2::req_url_path_append,
    c(list(httr2::request(FACTSET_BASE)), as.list(path))
  )

  request |>
    httr2::req_auth_basic(user, key) |>
    httr2::req_user_agent("Analisis-de-Riesgo/1.0 (R httr2)") |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i) |>
    httr2::req_timeout(90)
}

#' Perform a FactSet GET and return the parsed body.
#'
#' Translates the two failure modes that actually matter into messages that name
#' the cause: 401 is a bad credential, 403 is a missing entitlement. Those are
#' very different problems and the raw status code does not say so.
#'
#' @param path Path components below the content root.
#' @param query Named list of query parameters.
#' @return The parsed response body.
factset_get <- function(path, query = list()) {
  request <- do.call(
    httr2::req_url_query,
    c(list(factset_request(path)), query, list(.multi = "comma"))
  ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  response <- httr2::req_perform(request)
  status <- httr2::resp_status(response)

  if (status == 401) {
    stop(
      "FactSet rejected the credentials (401).\n",
      "  Check FACTSET_USERNAME_SERIAL and FACTSET_API_KEY in .env.",
      call. = FALSE
    )
  }
  if (status == 403) {
    stop(
      "FactSet accepted the credentials but the account is not entitled to ",
      paste(path, collapse = "/"), " (403).\n",
      "  A Workstation licence does not imply API entitlements. Ask your ",
      "FactSet representative which endpoints the key covers.",
      call. = FALSE
    )
  }
  if (status >= 400) {
    stop(
      "FactSet request failed (", status, ") for ",
      paste(path, collapse = "/"), ".",
      call. = FALSE
    )
  }

  httr2::resp_body_json(response, simplifyVector = FALSE)
}

#' Settle entitlement in one cheap call.
#'
#' Run this before building anything on the FactSet path. It requests a single
#' day of prices for one identifier, which is the smallest useful request the
#' Prices API accepts.
#'
#' @param id FactSet identifier. Defaults to the subject asset's.
#' @return TRUE on success; an informative error otherwise.
factset_ping <- function(id = asset_meta()$factset_id) {
  result <- factset_get(
    c("factset-prices", "v1", "prices"),
    list(
      ids = id,
      startDate = format(Sys.Date() - 7, "%Y-%m-%d"),
      endDate = format(Sys.Date(), "%Y-%m-%d"),
      frequency = "D"
    )
  )
  n <- length(result$data)
  message("FactSet reachable and entitled. ", n, " observations returned for ", id, ".")
  invisible(TRUE)
}

#' Quarterly fundamentals for one or more identifiers.
#'
#' @param ids FactSet identifiers.
#' @param metrics Metric codes.
#' @param start Fiscal period start, YYYY-MM-DD.
#' @param end Fiscal period end, YYYY-MM-DD.
#' @param periodicity "QTR" or "ANN".
#' @return A tidy data frame: id, metric, fiscal_period, fiscal_year, value.
fetch_factset_fundamentals <- function(ids = asset_meta()$factset_id,
                                       metrics = FACTSET_FUNDAMENTAL_METRICS,
                                       start = "2019-01-01",
                                       end = format(Sys.Date(), "%Y-%m-%d"),
                                       periodicity = "QTR") {
  body <- factset_get(
    c("factset-fundamentals", "v2", "fundamentals"),
    list(
      ids = paste(ids, collapse = ","),
      metrics = paste(metrics, collapse = ","),
      periodicity = periodicity,
      fiscalPeriodStart = start,
      fiscalPeriodEnd = end
    )
  )

  observations <- body$data
  if (is.null(observations) || length(observations) == 0) {
    stop(
      "FactSet returned no fundamentals for ", paste(ids, collapse = ", "), ".\n",
      "  Verify the identifiers in config/assets.yml and the metric codes.",
      call. = FALSE
    )
  }

  rows <- lapply(observations, function(row) {
    data.frame(
      id = row$requestId %||% row$fsymId %||% NA_character_,
      metric = row$metric %||% NA_character_,
      fiscal_period = row$fiscalPeriod %||% NA_character_,
      fiscal_year = row$fiscalYear %||% NA_integer_,
      period_end = row$fiscalEndDate %||% NA_character_,
      value = if (is.null(row$value)) NA_real_ else as.numeric(row$value),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  write_raw(out, "factset", "fundamentals.csv")
  out
}

#' Daily prices from FactSet.
#'
#' Not used by the committed datasets -- those come from public sources so the
#' repository reproduces without credentials. This exists as a cross-check on
#' the public price series and for universes Yahoo does not cover.
#'
#' @param ids FactSet identifiers.
#' @param start Start date.
#' @param end End date.
#' @return A tidy data frame: id, date, price, currency.
fetch_factset_prices <- function(ids = asset_meta()$factset_id,
                                 start = assets_config()$history_start,
                                 end = format(Sys.Date(), "%Y-%m-%d")) {
  body <- factset_get(
    c("factset-prices", "v1", "prices"),
    list(
      ids = paste(ids, collapse = ","),
      startDate = format(as.Date(start), "%Y-%m-%d"),
      endDate = format(as.Date(end), "%Y-%m-%d"),
      frequency = "D"
    )
  )

  rows <- lapply(body$data, function(row) {
    data.frame(
      id = row$requestId %||% NA_character_,
      date = as.Date(row$date),
      price = if (is.null(row$price)) NA_real_ else as.numeric(row$price),
      currency = row$currency %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  write_raw(out, "factset", "prices.csv")
  out[order(out$id, out$date), ]
}
