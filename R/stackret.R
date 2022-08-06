#' Stack Rets for ggplot
#'
#' Change the arrangement of multivariate data to generate suitable data for ggplot.
#'
#' @param rets multivariate data, arranged by column
#' @param date vector of common information for variables
#'
#' @return Suitable tibble data for plot by group in ggplot
#' @export
#'
#' @examples
#' names <- c("swan", "bear", "tiger")
#' date <- as.Date("2015-01-01") + days(0:179)
#' mu <- c(0.2, 0.08, 0.1)
#' sigma <- matrix(c(1, 0.25, -0.3, 0.25, 0.25, 0, -0.3, 0, 0.36), 3, 3)
#' allret <- rMvReturnSim(names, date, mu, sigma)
#' StackRet(allret, date)
StackRet <- function(rets, date) {
  rets <- as.matrix(rets)
  for (ii in seq_len(ncol(rets))) {
    if (ii == 1) {
      totret <- tibble(date = date, ret = rets[, ii], stock = colnames(rets)[ii])
    } else {
      totret_add <- tibble(date = date, ret = rets[, ii], stock = colnames(rets)[ii])
      totret <- rbind(totret, totret_add)
    }
  }
  return(totret)
}
