#' Construct Portfolio
#'
#' Construct four types portfolio with specificition and constraints.
#'
#' @param data multivariate returns, must be "timeSeries" type
#' @param method porofolio type, one of "fea", "minrisk", "globalminrisk" and "sharp"
#' @param spec specificition of portfolio
#' @param constraints constraints of trade
#'
#' @return A portfolio
#' @export
#'
#' @examples
#' names <- c("swan", "bear", "tiger")
#' date <- as.Date("2015-01-01") + days(0:179)
#' mu <- c(0.2, 0.08, 0.1)
#' sigma <- matrix(c(1, 0.25, -0.3, 0.25, 0.25, 0, -0.3, 0, 0.36), 3, 3)
#' allret <- rMvReturnSim(names, date, mu, sigma)
#' tsret <- as.timeSeries(allret)
#' feaSpec <- portfolioSpec()
#' setWeights(feaSpec) <- rep(1 / 3, times = 3)
#' InvestmentPortfolio(tsret, "fea", feaSpec)
InvestmentPortfolio <- function(data, method, spec, constraints = "LongOnly") {
  if (method == "fea") {
    fun <- feasiblePortfolio
  } else if (method == "minrisk") {
    fun <- efficientPortfolio
  } else if (method == "globminrisk") {
    fun <- minvariancePortfolio
  } else if (method == "sharp") {
    fun <- tangencyPortfolio
  } else {
    print("Wrong methods!")
    return
  }
  portfolio <- fun(
    data = data,
    spec = spec,
    constraints =
      constraints
  )
  return(portfolio)
}

#' Buy and Hold Backtest
#'
#' Backtest for the buy and hold with a fixed weights strategy.
#'
#' @param rets historic multivariate returns
#' @param weights holding weights of stock
#'
#' @return A backtest return series
#' @export
#'
#' @examples
#' names <- c("swan", "bear", "tiger")
#' date <- as.Date("2015-01-01") + days(0:179)
#' mu <- c(0.2, 0.08, 0.1)
#' sigma <- matrix(c(1, 0.25, -0.3, 0.25, 0.25, 0, -0.3, 0, 0.36), 3, 3)
#' allret <- rMvReturnSim(names, date, mu, sigma)
#' tsret <- as.timeSeries(allret)
#' FixBacktest(tsret, rep(1 / 3, 3))
FixBacktest <- function(rets, weights) {
  rets <- as.matrix(rets)
  testret <- rep(0, nrow(rets))
  for (ii in seq_len(ncol(rets))) {
    testret <- testret + rets[, ii] * weights[ii]
  }
  return(testret)
}
