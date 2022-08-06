#' Plot the Box Figure
#'
#' Plot the box figure beautifully with ggplot.
#'
#' @param data a tibble
#' @param mapping the mapping parameter of ggplot
#'
#' @return A box figure by ggplot
#' @export
#'
#' @examples
#' names <- c("swan", "bear", "tiger")
#' date <- as.Date("2015-01-01") + days(0:179)
#' mu <- c(0.2, 0.08, 0.1)
#' sigma <- matrix(c(1, 0.25, -0.3, 0.25, 0.25, 0, -0.3, 0, 0.36), 3, 3)
#' allret <- rMvReturnSim(names, date, mu, sigma)
#' totret <- StackRet(allret, date)
#' ggboxplot(totret, aes(x = stock, y = ret))
ggboxplot <- function(data, mapping) {
  gg <- ggplot(data = data) +
    geom_boxplot(mapping = mapping, fill = "steelblue") +
    theme_bw() +
    ylab(NULL) +
    xlab(NULL)
  return(gg)
}
