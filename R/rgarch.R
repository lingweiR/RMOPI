#' Simulate a Garch Series
#'
#' Simulate a Garch series given its data generate process without mean part.
#'
#' @param a0 vector of the start part
#' @param sigma20 vector of the initial variance \code{sigma2}
#' @param alpha the \code{alpha} parameter
#' @param beta the \code{beta} parameter
#' @param len the length, include defined \code{a0}
#'
#' @return A simulated garch series
#' @export
#'
#' @examples
#' rGarcha()
rGarcha <- function(a0 = rnorm(1, 0, 1), sigma20 = rnorm(1, 0, 1)^2, alpha = c(0.5, 0.5), beta = 0.25, len = 10) {
  m <- length(alpha) - 1
  s <- length(beta)
  na <- length(a0)
  ns <- length(sigma20)

  a <- c(a0, rep(0, len - na))
  a2 <- a^2
  sigma2 <- c(sigma20, rep(0, len - na))
  epilson <- rnorm(len - na)

  for (ii in seq_len(len - na)) {
    sigma2[ns + ii] <- sum(alpha * c(1, a2[(na + ii - 1):(na + ii - m)])) + sum(beta * sigma2[(ns + ii - 1):(ns + ii - s)])
    a[na + ii] <- sqrt(sigma2[ns + ii]) * epilson[ii]
    a2[na + ii] <- a[na + ii]^2
  }
  return(a)
}

#' Simulate a Garch Series
#'
#' Simulate a Garch series given its data generate process with mean part.
#'
#' @param u the mean series
#' @param a0 vector of the start part
#' @param sigma20 vector of the initial variance \code{sigma2}
#' @param alpha the \code{alpha} parameter
#' @param beta the \code{beta} parameter
#' @param len the length, include defined \code{a0}
#'
#' @return A simulated garch series
#' @export
#'
#' @examples
#' rGarch()
rGarch <- function(u = 0, a0 = rnorm(1, 0, 1), sigma20 = rnorm(1, 0, 1)^2, alpha = c(0.5, 0.5), beta = 0.25, len = 10) {
  a <- rGarcha(a0, sigma20, alpha, beta, len)
  r <- a + u
  return(r)
}
