#' Simulate Stocks Prices
#'
#' Simulate stocks prices following multivariate normal distribution.
#'
#' @param names vector of names
#' @param date vector of time, must be "Date" type
#' @param mu vector of \code{mu}
#' @param sigma vector of \code{sigma}
#'
#' @return Multivariate stock prices
#' @export
#'
#' @examples
#' names <- c("swan", "bear", "tiger")
#' date <- as.Date("2015-01-01") + days(0:179)
#' mu <- c(0.2, 0.08, 0.1)
#' sigma <- matrix(c(1, 0.25, -0.3, 0.25, 0.25, 0, -0.3, 0, 0.36), 3, 3)
#' rMvReturnSim(names, date, mu, sigma)
rMvReturnSim <- function(names, date, mu = rep(0, 2), sigma = matrix(c(1, 0.5, 0.5, 1, ), 2, 2)) {
  returns <- mvrnorm(n = length(date), mu, sigma)
  colnames(returns) <- names
  returns <- as.xts(returns, date)
  return(returns)
}
