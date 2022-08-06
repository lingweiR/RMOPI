#' VaR Calculation and Coverage Test
#'
#' Calculate VaR with three method and implement unconditional and conditional coverage test.
#'
#' @param data vector of returns
#' @param method the VaR method, one of "param", "hist" and "mc"
#' @param alpha the VaR confidence level
#' @param fun function calculating VaR, limited by \code{method}
#' @param ... the extra parameters of \code{fun}
#'
#' @return A list of VaR and coverage test outcome
#' @export
#'
#' @examples
#' swan <- rGarch(len = 30)
#' date <- as.Date("2015-01-01") + days(0:(length(swan) - 1))
#' tswan <- tibble(garch = swan, date = date)
#' tsswan <- as.xts(swan, date)
#' VaRSimTest(data = tsswan, method = "mc", alpha = 0.05, fun = rnorm, 100000, mean(tsswan), sd(tsswan))
VaRSimTest <- function(data, method, alpha, fun, ...) {
  if (method == "param") {
    varmodel <- fun(...)
    var <- quantile(varmodel, alpha)
  } else if (method == "hist") {
    var <- rep(fun(...), length(data))
  } else if (method == "mc") {
    varmodel <- fun(...)
    var <- rep(quantile(varmodel, alpha), length(data))
  }
  vartest <- VaRTest(alpha, actual = data, VaR = var)
  vartable <- sapply(c(1:12), function(x) vartest[[x]])
  names(vartable) <- rownames(as.data.frame(unlist(vartest)))
  vartable <- as.data.frame(vartable)
  return(list(var, vartable))
}
