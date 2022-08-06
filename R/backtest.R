#' Simulate Multivariate Stocks Prices Data
#'
#' Simulate multivariate prices for interconnected stocks with each price series following Geometric Brownian Motion (GBM).
#'
#' @param name vector of series names
#' @param len the length
#' @param start vector of start positions
#' @param mu vector of \code{mu}
#' @param sigma vector of \code{sigma}
#' @param digits integer deciding the number of decimal places
#'
#' @return A simulated multivariate GBM series with each series interconnected
#' @export
#'
#' @examples
#' rGbms(c("bear", "tiger"), len = 36)
rGbms <- function(name, len, start = c(1000, 1000), mu = rep(0.0001, 2), sigma = matrix(c(0.0002, 0.0001, 0.0001, 0.0002), 2, 2), digits = 2) {
  walks <- MASS::mvrnorm(n = len - 1, rep(0, nrow(sigma)), sigma)
  series <- matrix(rep(0, length(name) * len), ncol = length(name), nrow = len)
  colnames(series) <- name
  series[1, ] <- log(start)
  for (ii in seq_along(name)) {
    mulog <- mu[ii] - 0.5 * sigma[ii, ii]
    for (jj in 2:len) {
      series[jj, name[ii]] <- series[jj - 1, name[ii]] + mulog + walks[jj - 1, ii]
    }
  }
  series <- round(exp(series), digits)
  return(series)
}


#' Simulate Multivariate Stock Trade Data
#'
#' Simulate multivariate stock trade data with assumption that each stock price following Geometric Brownian Motion (GBM). And these prices are interconnected.
#'
#' @param name vector of names
#' @param time time vector of time, must be "Date" type
#' @param start vector of start positions
#' @param mu vector of \code{mu}
#' @param sigma vector of \code{sigma}
#' @param digits integer deciding the number of deciamal places
#'
#' @return A list of stock trade data with Open, High, Low and Close
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:29)
#' rTrades(c("swan", "bear"), date)
rTrades <- function(name, time, start = c(1000, 1000), mu = rep(0.0001, 2), sigma = matrix(c(0.0002, 0.0001, 0.0001, 0.0002), 2, 2), digits = 2) {
  trades <- list()
  for (ii in seq_along(name)) {
    trades[[ii]] <- matrix(rep(0, 4 * length(time)), ncol = 4, nrow = length(time))
    colnames(trades[[ii]]) <- c("Open", "High", "Low", "Close")
    trades[[ii]] <- xts::as.xts(trades[[ii]], time)
  }
  daystart <- start
  for (ii in seq_along(time)) {
    if (ii > 1) {
      for (kk in seq_along(name)) {
        daystart[kk] <- trades[[kk]][ii - 1, "Close"]
      }
    }
    dayseries <- rGbms(name, 10, daystart, mu, sigma, digits = 2)

    for (jj in seq_along(name)) {
      trades[[jj]][ii, "Open"] <- daystart[jj]
      trades[[jj]][ii, "High"] <- max(dayseries[, jj])
      trades[[jj]][ii, "Low"] <- min(dayseries[, jj])
      trades[[jj]][ii, "Close"] <- dayseries[10, jj]
    }
  }
  return(trades)
}



#' Calculate Sharp Ratio with stock prices
#'
#' Calculate sharp ratio of stock with running window.
#'
#' @param x vector of price
#' @param rf risk free rate
#' @param n the length of running window
#'
#' @return The sharp ratio series with length the same as x
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:29)
#' trade <- rTrade(date)
#' x <- trade$Close
#' Sharp(x)
Sharp <- function(x, rf = 0, n = 10) {
  ret <- timeSeries::returns0(x)
  u <- TTR::runMean(ret, n)
  sigma <- TTR::runSD(ret, n)
  sharp <- (u - rf) / sigma
  if (!is.null(dim(sharp))) {
    colnames(sharp) <- "Sharp"
  }
  return(sharp)
}



#' Calculate Useful Indicators for returns
#'
#' Calculate cumulative return, annualized return, max drawdown, annualized sharp ratio, calmar ratio, sortino ratio, alpha, beta and information ratio with returns.
#'
#' @param ret vector of return
#' @param rb return of market portfolio
#' @param rf risk free rate
#'
#' @return A matrix of return and risk indicators
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:249)
#' ret <- as.xts(rnorm(250), date)
#' rb <- as.xts(rep(0, 250), date)
#' RiskIndicators(ret, rb = rb, rf = 0)
RiskIndicators <- function(ret, rb, rf = 0) {
  risktab <- table.Arbitrary(ret,
    metrics = c(
      "Return.cumulative",
      "Return.annualized",
      "maxDrawdown",
      "SharpeRatio.annualized",
      "CalmarRatio",
      "SortinoRatio"
    ),
    metricsNames = c(
      "cumulative return",
      "annualized return",
      "max drawdow",
      "annualized sharp ratio",
      "calmar ratio",
      "sortino ratio"
    )
  )
  colnames(risktab) <- "strategy"
  risktab["alpha", "strategy"] <- CAPM.alpha(ret, Rb = rb, Rf = rf)
  risktab["beta", "strategy"] <- CAPM.beta(ret, Rb = rb, Rf = rf)
  risktab["information ratio", "strategy"] <- InformationRatio(ret, Rb = rb)
  return(risktab)
}
