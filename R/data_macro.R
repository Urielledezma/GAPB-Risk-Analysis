# Macroeconomic data.
#
# Three providers, deliberately ranked by what they cost the reader:
#
#   World Bank  no credentials, annual real GDP growth. This is the series the
#               stage 02 GDP comparison uses, so that comparison reproduces for
#               anyone who clones the repository.
#   INEGI       free token, quarterly GDP. Higher resolution, optional.
#   Banxico     free token, reference rate and FX. Context, optional.
#
# The optional two are skipped with a clear message when no token is present.
# Nothing downstream breaks.

WORLDBANK_BASE <- "https://api.worldbank.org/v2"
BANXICO_BASE <- "https://www.banxico.org.mx/SieAPIRest/service/v1"
INEGI_BASE <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml"

#' Annual real GDP growth from the World Bank.
#'
#' No credentials required.
#'
#' @param country ISO3 country code.
#' @param indicator World Bank indicator code.
#' @return A data frame with date, year and gdp_growth (percent).
fetch_gdp_worldbank <- function(country = NULL, indicator = NULL) {
  spec <- assets_config()$macro$gdp_annual
  country <- country %||% spec$country
  indicator <- indicator %||% spec$indicator

  response <- httr2::request(WORLDBANK_BASE) |>
    httr2::req_url_path_append("country", country, "indicator", indicator) |>
    httr2::req_url_query(format = "json", per_page = 500) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  observations <- response[[2]]
  if (is.null(observations) || length(observations) == 0) {
    stop("World Bank returned no observations for ", indicator, ".", call. = FALSE)
  }

  rows <- lapply(observations, function(row) {
    if (is.null(row$value)) {
      return(NULL)
    }
    data.frame(
      year = as.integer(row$date),
      gdp_growth = as.numeric(row$value),
      stringsAsFactors = FALSE
    )
  })
  gdp <- do.call(rbind, Filter(Negate(is.null), rows))
  gdp$date <- as.Date(paste0(gdp$year, "-12-31"))
  gdp <- gdp[order(gdp$year), c("date", "year", "gdp_growth")]
  rownames(gdp) <- NULL
  gdp
}

#' A Banxico SIE series.
#'
#' @param series SIE series id, e.g. "SF43718".
#' @param from Start date.
#' @param to End date.
#' @return A data frame with date, series and value.
fetch_banxico_series <- function(series,
                                 from = as.Date("2000-01-01"),
                                 to = Sys.Date()) {
  token <- require_env(
    "BANXICO_SIE_TOKEN",
    "Request one free at https://www.banxico.org.mx/SieAPIRest/service/v1/token"
  )

  response <- httr2::request(BANXICO_BASE) |>
    httr2::req_url_path_append(
      "series", series, "datos",
      format(as.Date(from), "%Y-%m-%d"),
      format(as.Date(to), "%Y-%m-%d")
    ) |>
    httr2::req_headers(`Bmx-Token` = token) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  observations <- response$bmx$series[[1]]$datos
  if (is.null(observations) || length(observations) == 0) {
    stop("Banxico returned no observations for series ", series, ".", call. = FALSE)
  }

  rows <- lapply(observations, function(row) {
    value <- suppressWarnings(as.numeric(row$dato))
    if (is.na(value)) {
      return(NULL)
    }
    data.frame(
      date = as.Date(row$fecha, format = "%d/%m/%Y"),
      series = series,
      value = value,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out[order(out$date), ]
}

#' An INEGI BIE indicator.
#'
#' @param indicator BIE indicator id.
#' @param geography INEGI geographic area code. 0700 is the national total.
#' @return A data frame with period, date and value.
fetch_inegi_indicator <- function(indicator, geography = "0700") {
  token <- require_env(
    "INEGI_API_TOKEN",
    "Request one free at https://www.inegi.org.mx/servicios/api_indicadores.html"
  )

  response <- httr2::request(INEGI_BASE) |>
    httr2::req_url_path_append(
      "INDICATOR", indicator, "es", geography, "false", "BIE", "2.0", token
    ) |>
    httr2::req_url_query(type = "json") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  observations <- response$Series[[1]]$OBSERVATIONS
  if (is.null(observations) || length(observations) == 0) {
    stop("INEGI returned no observations for indicator ", indicator, ".", call. = FALSE)
  }

  rows <- lapply(observations, function(row) {
    value <- suppressWarnings(as.numeric(row$OBS_VALUE))
    if (is.na(value)) {
      return(NULL)
    }
    data.frame(
      period = row$TIME_PERIOD,
      value = value,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out$date <- quarter_end_date(out$period)
  out[order(out$date), c("date", "period", "value")]
}

#' Convert an INEGI "YYYY/QQ" period label to the quarter-end date.
#'
#' @param period Character vector of period labels.
#' @return A Date vector.
quarter_end_date <- function(period) {
  parts <- strsplit(as.character(period), "[/-]")
  as.Date(vapply(parts, function(p) {
    year <- p[1]
    quarter <- suppressWarnings(as.integer(gsub("\\D", "", p[length(p)])))
    if (is.na(quarter) || quarter < 1 || quarter > 4) {
      return(paste0(year, "-12-31"))
    }
    c(
      paste0(year, "-03-31"),
      paste0(year, "-06-30"),
      paste0(year, "-09-30"),
      paste0(year, "-12-31")
    )[quarter]
  }, character(1)))
}

# Null-coalescing helper. Base R has no ??.
`%||%` <- function(x, y) if (is.null(x)) y else x
