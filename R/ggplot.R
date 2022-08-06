#' Plot the Time Series
#'
#' Plot the time series data beautifully with ggplot.
#'
#' @param data a tibble
#' @param mapping the mapping parameter of ggplot
#' @param date_labels the x label
#' @param date_breaks the period of the x label
#'
#' @return A ggplot figure of the time series
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:180)
#' thero <- returns(rGbm("thero", date))[-1]
#' tthero <- tibble(x = date[-1], y = thero)
#' gglineplot(tthero, aes(x, y), "%Y/%m", "1 months")
gglineplot <- function(data, mapping = aes(x, y), date_labels = "%Y/%m/%d", date_breaks = "2 weeks") {
  gg <- ggplot(data = data, mapping = mapping) +
    geom_line(color = "steelblue") +
    geom_point(shape = 2, color = "red") +
    scale_x_date(date_labels = date_labels, date_breaks = date_breaks) +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL)
  return(gg)
}


#' Plot the Histogram Figure
#'
#' Plot the histgram figure beautifully with ggplot.
#'
#' @param data a tibble
#' @param mapping the mapping parameter of ggplot
#' @param bins the number of bins
#'
#' @return A histogram figure by ggplot
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:180)
#' thero <- returns(rGbm("thero", date))[-1]
#' tthero <- tibble(x = date[-1], y = thero)
#' gghistplot(tthero, aes(x = thero, y = stat(density)), bins = 20)
gghistplot <- function(data, mapping = aes(x = x, y = stat(density)), bins = 10) {
  ggplot(data = data, mapping) +
    geom_histogram(bins = bins, fill = "steelblue") +
    geom_density(color = "red") +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL)
}
