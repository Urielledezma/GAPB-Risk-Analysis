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

#' The public address FactSet sees this machine as.
#'
#' An API key is bound to an IP allowlist, and a request from an address outside
#' it is refused with the same undiscriminating 401 as a wrong key -- measured
#' 2026-08-27: a correct key, a corrupted key, a wrong serial and no credentials
#' at all all return HTTP 401 with the body "Authentication Failed". The address
#' is therefore the first thing to check, and on a dynamic or campus connection
#' it changes without warning.
#'
#' @return The address as a string, or NA if it cannot be determined.
factset_caller_ip <- function() {
  tryCatch(
    {
      response <- httr2::request("https://api.ipify.org") |>
        httr2::req_timeout(15) |>
        httr2::req_perform()
      trimws(httr2::resp_body_string(response))
    },
    error = function(e) NA_character_
  )
}

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
    address <- factset_caller_ip()
    stop(
      "FactSet refused the request (401, \"Authentication Failed\").\n",
      "  This one status covers two unrelated faults and does not say which:\n",
      "    1. the calling address is outside the key's IP Range allowlist;\n",
      "    2. the key or the username-serial is wrong or has been regenerated.\n",
      "  This machine is currently seen as ",
      if (is.na(address)) "an address that could not be determined" else address,
      ".\n",
      "  Confirm that address falls inside the IP Range on the key at\n",
      "  https://developer.factset.com/api-authentication before touching the\n",
      "  credentials -- a dynamic or campus connection changes it without notice.",
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

# Field-name candidates for the Prices response.
#
# The parser reads whichever of these the API actually returns rather than
# assuming one shape. FactSet's documented field names have moved between
# endpoint versions, and guessing wrong here produces a column of NA that looks
# like missing data instead of a mapping error. analysis/00_probe_factset.R
# prints the real keys; once they are confirmed, narrow these lists.
FACTSET_PRICE_FIELDS <- list(
  date = c("date", "priceDate"),
  close = c("price", "priceClose", "closePrice"),
  open = c("priceOpen", "openPrice", "open"),
  high = c("priceHigh", "highPrice", "high"),
  low = c("priceLow", "lowPrice", "low"),
  volume = c("volume", "tradeVolume"),
  currency = c("currency", "currencyCode"),
  id = c("requestId", "fsymId", "id")
)

#' Pull the first field present from a response row.
#'
#' @param row A parsed JSON object.
#' @param candidates Field names to try, in order of preference.
#' @param default Value when none is present.
#' @return The value found, or the default.
pick_field <- function(row, candidates, default = NA) {
  for (name in candidates) {
    if (!is.null(row[[name]])) {
      return(row[[name]])
    }
  }
  default
}

#' Daily prices from FactSet.
#'
#' The primary market data path. Returns the same tidy schema as the public
#' loader so the two are interchangeable downstream, with a source column so
#' every row records where it came from.
#'
#' A caveat that matters for every return in this project: `adjust` controls
#' split and spin-off adjustment, not dividend adjustment. Until the probe
#' confirms which series the entitled endpoint returns, `adjusted` mirrors the
#' close and the total-return treatment is an open question rather than a
#' settled one. Getting this wrong understates long-horizon returns without
#' changing volatility much, which makes it hard to notice.
#'
#' @param ids FactSet identifiers.
#' @param tickers_out Tickers to label the rows with, aligned with ids.
#' @param start Start date.
#' @param end End date.
#' @param adjust Split and spin-off adjustment mode.
#' @return A tidy data frame matching the public loader's schema.
fetch_factset_prices <- function(ids = asset_meta()$factset_id,
                                 tickers_out = asset_meta()$ticker,
                                 start = assets_config()$history_start,
                                 end = format(Sys.Date(), "%Y-%m-%d"),
                                 adjust = "SPLIT_SPINOFF") {
  body <- factset_get(
    c("factset-prices", "v1", "prices"),
    list(
      ids = paste(ids, collapse = ","),
      startDate = format(as.Date(start), "%Y-%m-%d"),
      endDate = format(as.Date(end), "%Y-%m-%d"),
      frequency = "D",
      adjust = adjust
    )
  )

  observations <- body$data
  if (is.null(observations) || length(observations) == 0) {
    stop(
      "FactSet returned no prices for ", paste(ids, collapse = ", "), ".\n",
      "  Confirm the identifiers in config/assets.yml with ",
      "analysis/00_probe_factset.R.",
      call. = FALSE
    )
  }

  id_to_ticker <- stats::setNames(tickers_out, ids)

  rows <- lapply(observations, function(row) {
    id <- as.character(pick_field(row, FACTSET_PRICE_FIELDS$id, NA_character_))
    close <- as.numeric(pick_field(row, FACTSET_PRICE_FIELDS$close, NA_real_))

    data.frame(
      date = as.Date(substr(as.character(
        pick_field(row, FACTSET_PRICE_FIELDS$date, NA_character_)
      ), 1, 10)),
      ticker = unname(id_to_ticker[id]) %||% id,
      open = as.numeric(pick_field(row, FACTSET_PRICE_FIELDS$open, NA_real_)),
      high = as.numeric(pick_field(row, FACTSET_PRICE_FIELDS$high, NA_real_)),
      low = as.numeric(pick_field(row, FACTSET_PRICE_FIELDS$low, NA_real_)),
      close = close,
      adjusted = close,
      volume = as.numeric(pick_field(row, FACTSET_PRICE_FIELDS$volume, NA_real_)),
      source = "factset",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[!is.na(out$date), ]
  write_raw(out, "factset", "prices.csv")
  out[order(out$ticker, out$date), ]
}
