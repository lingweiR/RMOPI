#' Summary Statistics of Vector
#'
#' Calculate useful statistics for an univariate data.
#'
#' @param data vector of observations
#' @param digits integer deciding the number of decimal places
#'
#' @return A tibble of statistics, including min, max, mean, sd, Q25, Q50, Q75, kurt, Skew, n, na
#' @export
#'
#' @examples
#' swan <- rGarch(len = 180)
#' DescribeVector(swan)
DescribeVector <- function(data, digits = 2) {
  datana <- sum(is.na(data))
  data <- na.omit(data)
  stat <- tibble(
    min = min(data),
    max = max(data),
    mean = mean(data),
    sd = sd(data),
    Q25 = quantile(data, 0.25),
    Q50 = quantile(data, 0.5),
    Q75 = quantile(data, 0.75),
    kurt = timeDate::kurtosis(data)[1],
    skew = timeDate::skewness(data)[1],
    n = length(data),
    na = datana
  )
  return(round(stat, digits))
}

#' Summary Statistics
#'
#' Calculate useful statistics for an multivariate data.
#'
#' @param data vector of observations
#' @param digits integer deciding the number of decimal places
#'
#' @return A tibble of statistics, including min, max, mean, sd, Q25, Q50, Q75, kurt, Skew, n, na
#' @export
#'
#' @examples
#' swan <- rGarch(len = 180)
#' Describe(tibble(a1 = swan, a2 = swan + 1), 2)
Describe <- function(data, digits = 2) {
  sumstats <- sapply(colnames(data), function(x) DescribeVector(as.matrix(data[x]), digits))
  return(t(sumstats))
}
