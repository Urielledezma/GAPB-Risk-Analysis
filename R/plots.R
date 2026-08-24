# Chart theme.
#
# One house style, applied everywhere, so the five reports read as one document
# rather than five. Charts are static ggplot2 output rendered into the Quarto
# site, and the site commits to a single light surface -- a dark toggle would
# leave the rendered PNGs light-on-dark, which is worse than not offering one.
#
# The categorical palette is the first six slots of a validated eight-slot
# order, checked with the accompanying validator against a white surface:
#
#   lightness band     PASS   all six inside L 0.43-0.77
#   chroma floor       PASS   all six >= 0.1
#   CVD separation     PASS   worst adjacent pair dE 9.1 (protan)
#   normal vision      PASS   worst adjacent pair dE 19.6
#   contrast           WARN   aqua, yellow and magenta sit below 3:1
#
# The contrast warning is not dismissable: it obligates relief. Every chart with
# more than one series therefore carries either direct labels or an accompanying
# table, which is why label_series_end() exists below and why the report
# templates pair each multi-series figure with its data table.
#
# Hue order is a colour-vision-deficiency safety mechanism, not decoration. Slots
# are assigned in fixed order and never cycled; a seventh series would fold into
# an "other" grouping or become a small multiple.

RISK_PALETTE <- c(
  "#2a78d6", # 1 blue
  "#eb6834", # 2 orange
  "#1baf7a", # 3 aqua
  "#eda100", # 4 yellow
  "#e87ba4", # 5 magenta
  "#008300"  # 6 green
)

RISK_INK <- list(
  surface = "#ffffff",
  primary = "#0b0b0b",
  secondary = "#52514e",
  muted = "#898781",
  gridline = "#e1e0d9",
  baseline = "#c3c2b7"
)

# Reserved for state, never for a series.
RISK_STATUS <- c(
  good = "#0ca30c",
  warning = "#fab219",
  serious = "#ec835a",
  critical = "#d03b3b"
)

#' Assign palette slots to the universe, in fixed order.
#'
#' The subject asset always takes slot 1. Colour follows the entity, so a chart
#' that drops three tickers does not repaint the survivors.
#'
#' @param assets Tickers to colour. Defaults to the configured universe.
#' @return A named character vector of hex colours.
asset_colours <- function(assets = tickers()) {
  ordered <- unique(c(subject(), sort(setdiff(assets, subject()))))
  ordered <- ordered[ordered %in% assets]
  if (length(ordered) > length(RISK_PALETTE)) {
    stop(
      "The palette has ", length(RISK_PALETTE), " slots and ", length(ordered),
      " series were requested. Fold the tail into an 'other' group or facet ",
      "instead of generating a new hue.",
      call. = FALSE
    )
  }
  stats::setNames(RISK_PALETTE[seq_along(ordered)], ordered)
}

#' Discrete colour scale keyed to the asset universe.
#'
#' @param ... Passed to ggplot2::scale_colour_manual().
#' @return A ggplot2 scale.
scale_colour_asset <- function(...) {
  ggplot2::scale_colour_manual(values = asset_colours(), ...)
}

#' Discrete fill scale keyed to the asset universe.
#'
#' @param ... Passed to ggplot2::scale_fill_manual().
#' @return A ggplot2 scale.
scale_fill_asset <- function(...) {
  ggplot2::scale_fill_manual(values = asset_colours(), ...)
}

#' The house ggplot2 theme.
#'
#' Recessive chrome: hairline horizontal grid, no vertical grid, no panel
#' border, no box around the legend. Text wears ink tokens rather than the
#' series colour, so identity is carried by the mark beside the label and never
#' by the label itself.
#'
#' @param base_size Base font size in points.
#' @return A ggplot2 theme.
theme_risk <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = RISK_INK$secondary),
      plot.title = ggplot2::element_text(
        colour = RISK_INK$primary, face = "bold",
        size = ggplot2::rel(1.15), margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = RISK_INK$secondary, size = ggplot2::rel(0.95),
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        colour = RISK_INK$muted, size = ggplot2::rel(0.8), hjust = 0
      ),
      axis.title = ggplot2::element_text(colour = RISK_INK$muted, size = ggplot2::rel(0.9)),
      axis.text = ggplot2::element_text(colour = RISK_INK$muted),
      axis.line.x = ggplot2::element_line(colour = RISK_INK$baseline, linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(colour = RISK_INK$gridline, linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = RISK_INK$surface, colour = NA),
      plot.background = ggplot2::element_rect(fill = RISK_INK$surface, colour = NA),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = RISK_INK$primary, face = "bold"),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

#' Direct-label the end of each series.
#'
#' This is the relief the palette's contrast warning obligates. Use it on any
#' multi-series line chart; a legend alone is not sufficient for the three slots
#' that sit below 3:1 against the surface.
#'
#' @param data A data frame with the plotted series.
#' @param x X column name.
#' @param y Y column name.
#' @param group Series column name.
#' @param size Label size.
#' @return A ggplot2 layer.
label_series_end <- function(data, x = "date", y = "value", group = "ticker", size = 3) {
  last <- do.call(
    rbind,
    lapply(split(data, data[[group]]), function(one) one[which.max(one[[x]]), ])
  )
  ggplot2::geom_text(
    data = last,
    ggplot2::aes(
      x = .data[[x]], y = .data[[y]], label = .data[[group]],
      colour = .data[[group]]
    ),
    hjust = -0.1, size = size, show.legend = FALSE
  )
}

#' Save a figure to outputs/figures/.
#'
#' outputs/ is untracked: figures are regenerated from the committed data rather
#' than versioned alongside it.
#'
#' @param plot A ggplot object.
#' @param name File name without extension.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution.
#' @return Invisibly, the path written.
save_figure <- function(plot, name, width = 8, height = 4.5, dpi = 200) {
  path <- proj_path("outputs", "figures", paste0(name, ".png"))
  ensure_dir(dirname(path))
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi, bg = RISK_INK$surface)
  invisible(path)
}

#' Percent formatter for axes and tables.
#'
#' @param x Numeric vector of proportions.
#' @param digits Decimal places.
#' @return A character vector.
fmt_pct <- function(x, digits = 2) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

#' Currency formatter for MXN amounts.
#'
#' @param x Numeric vector.
#' @param digits Decimal places.
#' @return A character vector.
fmt_mxn <- function(x, digits = 0) {
  paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
}
