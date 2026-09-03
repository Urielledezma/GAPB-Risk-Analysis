# Shared setup for every report.
#
# Sourced from the first chunk of each .qmd. Loads the analytics library, the
# plotting theme and the chunk defaults, so no report restates any of them.
#
# Quarto executes each document from this directory, which is why the library
# path is relative. proj_path() takes over from there and resolves everything
# else from the project root.

source(file.path("..", "R", "utils_io.R"))
source_lib()

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

ggplot2::theme_set(theme_risk())

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 4.5,
  dpi = 200,
  fig.align = "center",
  dev = "png"
)

options(
  scipen = 999,
  digits = 4,
  knitr.kable.NA = "—"
)

#' Render a data frame as a report table.
#'
#' Every multi-series figure in this project is paired with its table: the chart
#' palette carries a sub-3:1 contrast warning on three of its six slots, and a
#' readable table is one of the two accepted forms of relief.
#'
#' @param data A data frame.
#' @param caption Table caption.
#' @param digits Decimal places.
#' @return A knitr kable.
report_table <- function(data, caption = NULL, digits = 4) {
  column_labels <- c(
    ticker = "Ticker", name = "Emisora", series = "Serie",
    sector = "Sector", industry = "Industria",
    factset_id = "Identificador FactSet", year = "Año",
    source = "Fuente", rows = "Filas", first = "Primera fecha",
    last = "Última fecha", inicio = "Inicio", fin = "Fin",
    observaciones = "Observaciones", max_drawdown = "Drawdown máximo",
    peak_date = "Fecha del máximo", trough_date = "Fecha del mínimo",
    recovery_date = "Fecha de recuperación",
    asset_return = "Rendimiento del activo",
    gdp_growth = "Crecimiento real del PIB", n = "Observaciones",
    mean_daily = "Media diaria", vol_daily = "Volatilidad diaria",
    mean_annual = "Media anualizada",
    vol_annual = "Volatilidad anualizada", skewness = "Asimetría",
    kurtosis = "Curtosis", excess_kurtosis = "Exceso de curtosis",
    statistic = "Estadístico", p_value = "Valor p",
    reject_normality = "Rechaza normalidad", mean = "Media",
    std_error = "Error estándar", df = "Grados de libertad",
    conf_low = "Límite inferior", conf_high = "Límite superior",
    reject_zero_mean = "Rechaza media cero", horizon = "Horizonte",
    s0 = "Precio inicial (MXN)", expected = "Esperado (MXN)",
    median = "Mediana (MXN)", lower = "Límite inferior (MXN)",
    upper = "Límite superior (MXN)", confidence = "Confianza",
    interval_width = "Amplitud del intervalo (MXN)", criterio = "Criterio",
    lambda = "Lambda", alpha = "Alfa", price = "Precio (MXN)",
    weight = "Peso", value = "Valor (MXN)", shares = "Acciones",
    method = "Método", metodo = "Método", var_return = "VaR (rendimiento)",
    var_currency = "VaR (MXN)", var_pct = "VaR (%)",
    expected_breaches = "Excedencias esperadas",
    observed_breaches = "Excedencias observadas",
    breach_rate = "Tasa de excedencias", lr_statistic = "Estadístico LR",
    reject_model = "Rechaza el modelo", n_simulations = "Simulaciones",
    es_return = "ES (rendimiento)", es_currency = "ES (MXN)",
    es_pct = "ES (%)"
  )

  table_data <- data
  year_columns <- names(table_data) %in% c("year", "Año")
  table_data[year_columns] <- lapply(
    table_data[year_columns],
    function(x) if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else x
  )

  labels <- unname(column_labels[names(table_data)])
  missing_labels <- is.na(labels)
  labels[missing_labels] <- names(table_data)[missing_labels]
  names(table_data) <- labels

  is_numeric_display <- function(x) {
    if (is.numeric(x)) {
      return(TRUE)
    }
    if (!is.character(x)) {
      return(FALSE)
    }

    values <- trimws(x[!is.na(x) & nzchar(trimws(x))])
    length(values) > 0L && all(grepl(
      "^-?[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?(?:%|x)?$",
      values,
      perl = TRUE
    ))
  }

  alignment <- ifelse(
    vapply(table_data, is_numeric_display, logical(1)),
    "r",
    "l"
  )

  knitr::kable(
    table_data,
    caption = caption,
    digits = digits,
    format.args = list(big.mark = ","),
    align = alignment
  )
}
