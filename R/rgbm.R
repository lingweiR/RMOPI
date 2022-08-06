#' Simulate a single stock price series
#'
#' Simulate an univariate series following Geometric Brownian Motion (GBM).
#'
#' @param len the length
#' @param start the start position
#' @param mu the \code{mu} parameter of GBM
#' @param sigma the \code{sigma} parameter of GBM
#'
#' @return a simulated univariate GBM series
#' @export
#'
#' @examples
#' rGbmSingle(100)
rGbmSingle <- function(len, start = 100, mu = 0.01, sigma = 0.02) {
  series <- rep(0, len)
  mulog <- mu - 0.5 * sigma^2
  walk <- rnorm(len - 1)
  series[1] <- log(start)
  for (ii in 2:len) {
    series[ii] <- series[ii - 1] + mulog + sigma * walk[ii - 1]
  }
  series <- round(exp(series), digits = 2)
  return(series)
}

#' Simulate prices series of stocks
#'
#' Simulate an multivariate series following Geometric Brownian Motion (GBM)
#'
#' @param name vector of series names
#' @param time vector of time, must be a "Date" type variable
#' @param start vector of start positions
#' @param mu vector of \code{mu}
#' @param sigma vector of \code{sigma}
#'
#' @return a simulated multivariate GBM series
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:29)
#' rGbm(c("bear", "tiger", "swan"), date)
rGbm <- function(name, time, start = 100, mu = 0.01, sigma = 0.02) {
  series <- matrix(rep(0, length(name) * length(time)), ncol = length(name), nrow = length(time))
  colnames(series) <- name

  rep_fun <- function(x) {
    if (length(x) == 1L) {
      return(rep(x, length(name)))
    }
  }
  start <- rep_fun(start)
  mu <- rep_fun(mu)
  sigma <- rep_fun(sigma)

  for (ii in seq_along(name)) {
    series[, name[ii]] <- rGbmSingle(length(time), start[ii], mu[ii], sigma[ii])
  }
  series <- as.xts(series, time)
  return(series)
}


#' Simulate stock trade data
#'
#' Simulate stock trade data with assumption that the stock price following Geometric Brownian Motion (GBM).
#'
#' @param time time vector of time, must be a "Date" type variable
#' @param start the start position
#' @param mu the \code{mu} parameter of GBM
#' @param sigma the \code{sigma} parameter of GBM
#'
#' @return Stock trade data with Open, High, Low and Close
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:29)
#' rTrade(date)
rTrade <- function(time, start = 100, mu = 0.0001, sigma = 0.0002) {
  series <- matrix(rep(0, 4 * length(time)), ncol = 4, nrow = length(time))
  colnames(series) <- c("Open", "High", "Low", "Close")
  for (ii in seq_along(time)) {
    if (ii == 1) {
      series[ii, "Open"] <- start
    } else {
      series[ii, "Open"] <- series[ii - 1, "Close"]
    }
    dayseries <- rGbmSingle(36, series[ii, "Open"], mu, sigma)
    series[ii, "High"] <- max(dayseries)
    series[ii, "Low"] <- min(dayseries)
    series[ii, "Close"] <- dayseries[36]
  }
  series <- as.xts(series, time)
  return(series)
}
