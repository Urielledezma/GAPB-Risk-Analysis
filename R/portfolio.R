# Portfolio construction and volatility.
#
# The reference portfolio holds an equal currency amount in each asset of the
# universe. Equal WEIGHTS, not equal share counts -- the distinction matters
# because the six issuers trade at very different price levels, and a portfolio
# of one hundred shares each is a concentrated bet on whichever name is most
# expensive.

#' Build a portfolio from a price panel.
#'
#' @param prices A tidy price panel.
#' @param as_of Valuation date. Defaults to the last date on which every asset
#'   traded.
#' @param position Currency amount per asset.
#' @param weights Optional named weight vector. Defaults to equal weights.
#' @param price_col Price column used to value shares, by default the configured price field.
#' @return A list with the valuation date, the holdings table, the total value
#'   and the weight vector.
build_portfolio <- function(prices,
                            as_of = NULL,
                            position = params()$var$position_mxn,
                            weights = NULL,
                            price_col = params()$conventions$price_field) {
  assets <- sort(unique(prices$ticker))

  if (is.null(as_of)) {
    dates_per_asset <- split(prices$date, prices$ticker)
    common <- Reduce(intersect, dates_per_asset)
    as_of <- max(as.Date(common, origin = "1970-01-01"))
  }
  as_of <- as.Date(as_of)

  snapshot <- prices[prices$date == as_of, c("ticker", price_col)]
  names(snapshot)[2] <- "price"
  snapshot <- snapshot[match(assets, snapshot$ticker), ]

  invalid <- !is.finite(snapshot$price) | snapshot$price <= 0
  if (any(invalid)) {
    stop(
      "No positive finite price on ", as_of, " for: ",
      paste(assets[invalid], collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(weights)) {
    weights <- rep(1 / length(assets), length(assets))
    names(weights) <- assets
  }
  weights <- weights[assets]

  total_value <- position * length(assets)
  snapshot$weight <- as.numeric(weights)
  snapshot$value <- total_value * snapshot$weight
  snapshot$shares <- snapshot$value / snapshot$price

  list(
    as_of = as_of,
    holdings = snapshot,
    value = total_value,
    weights = weights
  )
}

#' Portfolio volatility from weights and a covariance matrix.
#'
#'   sigma_p = sqrt( w' Sigma w )
#'
#' @param weights Named weight vector.
#' @param covariance Covariance matrix.
#' @return The portfolio standard deviation.
portfolio_sigma <- function(weights, covariance) {
  weights <- as.numeric(weights[colnames(covariance)])
  sqrt(as.numeric(t(weights) %*% covariance %*% weights))
}
