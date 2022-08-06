#' Plot the Acf Figure
#'
#' Plot the Acf figure with observations of a single variable beautifully.
#'
#' @param data vector of observations
#' @param lag the maximum lag to calculate the acf
#'
#' @return A ggplot figure of the acf
#' @export
#'
#' @examples
#' swan <- rGarch(len = 180)
#' ggacf(swan^2, 20)
ggacf <- function(data, lag = 10) {
  tacf <- tibble(acf = c(acf(data, lag, plot = FALSE)$acf), acflag = c(0:lag))
  acfsd <- 1 / sqrt(length(data))
  gg <- ggplot(data = tacf, mapping = aes(x = acflag, y = acf)) +
    geom_col(fill = "steelblue", width = 0.2) +
    geom_hline(yintercept = c(2 * acfsd, -2 * acfsd), linetype = "dashed", color = "red") +
    theme_bw() +
    xlab("Lag") +
    ylab("Acf") +
    scale_x_continuous(breaks = c(0:lag))
  return(gg)
}

#' Plot the Pacf Figure
#'
#' Plot the Pacf figure with observations of a single variable beautifully.
#'
#' @param data vector of observations
#' @param lag the maximum lag to calculate the pacf
#'
#' @return A ggplot figure of the pacf
#' @export
#'
#' @examples
#' swan <- rGarch(len = 180)
#' ggpacf(swan^2, 20)
ggpacf <- function(data, lag = 10) {
  tpacf <- tibble(pacf = c(pacf(data, lag, plot = FALSE)$acf), pacflag = c(1:lag))
  pacfsd <- 1 / sqrt(length(data))
  gg <- ggplot(data = tpacf, mapping = aes(x = pacflag, y = pacf)) +
    geom_col(fill = "steelblue", width = 0.2) +
    geom_hline(yintercept = c(2 * pacfsd, -2 * pacfsd), linetype = "dashed", color = "red") +
    theme_bw() +
    xlab("Lag") +
    ylab("Pacf") +
    scale_x_continuous(breaks = c(1:lag))
  return(gg)
}
