#' F_k Coefficients
#'
#' Calculate the F_k coefficients for the cos method.
#'
#' @param Chf the characteristic function
#' @param N the number of cos term for summation
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#'
#' @return A vector of F_k coefficients
#' @export
#'
#' @examples
#' N <- 32
#' a <- -6.0
#' b <- 6.0
#' F_k(StNormChf, N, a, b)
F_k <- function(Chf, N, a, b) {
  k <- 0:(N - 1)
  u <- k * pi / (b - a)
  fk <- Re(Chf(u) * exp(-1i * u * a)) * 2 / (b - a)
  fk[1] <- fk[1] * 0.5
  return(fk)
}


#' Distribution Recovery with the COS method
#'
#' Restore the distribution with the characteristic function through the COS method.
#'
#' @param x vector of observations
#' @param Chf the characteristic function
#' @param N the number of cos term for summation
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#'
#' @return The approximated probability density of x
#' @export
#'
#' @examples
#' N <- 32
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' a <- -6.0
#' b <- 6.0
#' CosPdfRecovery(x, StNormChf, N, a, b)
CosPdfRecovery <- function(x, Chf, N, a, b) {
  k <- 0:(N - 1)
  u <- k * pi / (b - a)
  fk <- F_k(Chf, N, a, b)
  fx <- function(x1) {
    sum(fk * cos(u * (x1 - a)))
  }
  f_x <- sapply(x, fx)
  return(f_x)
}


#' The Characteristic Function of Normal Distribution
#'
#' @param u observation
#' @param mu the \code{mu} parameter
#' @param sigma the \code{sigma} parameter
#'
#' @return The value of Characteristic Function
#' @export
#'
#' @examples
#' NormChf(1)
NormChf <- function(u, mu = 0, sigma = 1) {
  return(exp(1i * mu * u - 0.5 * sigma^2 * u^2))
}



#' The Characteristic Function of Standard Normal Distribution
#'
#' @param u observation
#'
#' @return The value of Characteristic Function
#' @export
#'
#' @examples
#' StNormChf(1)
StNormChf <- function(u) {
  NormChf(u, mu = 0, sigma = 1)
}



#' Plot the Probability Density Function
#'
#' Plot the p.d.f function for the univariate distribution with x and y.
#'
#' @param data a tiible contains x and y
#' @param mapping the mapping parameter of ggplot
#'
#' @return A ggplot figure of the probability density function
#' @export
#'
#' @examples
#' N <- 32
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' a <- -6.0
#' b <- 6.0
#' f_x <- CosPdfRecovery(x, StNormChf, N, a, b)
#' tnorm <- tibble(x = x, y = f_x)
#' PdfSinglePlot(tnorm)
PdfSinglePlot <- function(data, mapping = aes(x, y)) {
  gg <- ggplot(data = data) +
    geom_point(mapping = mapping, col = "steelblue") +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw()
  return(gg)
}



#' Distribution Recovery with the COS method for Different parameters
#'
#' Restore the distribution with the COS method under different parameters settings for error analysis.
#'
#' @param x vector of observations
#' @param Chf the characteristic function
#' @param N the number of cos term for summation
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#'
#' @return A matrix that contains restored p.d.f. with different parameters
#' @export
#'
#' @examples
#' N <- 2**(1:6)
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' a <- -10.0
#' b <- 10.0
#' CosPdfMulti(x, StNormChf, N, a, b)
CosPdfMulti <- function(x, Chf, N, a, b) {
  if (length(a) == 1L && length(b) == 1L) {
    f_x <- sapply(N, CosPdfRecovery, x = x, Chf = Chf, a = a, b = b)
  } else if (length(a) == length(b) && length(N) == 1L) {
    f_x <- matrix(0, nrow = length(x), ncol = length(a))
    for (kk in 1:length(a)) {
      f_x[, kk] <- CosPdfRecovery(x, Chf, N, a[kk], b[kk])
    }
  } else {
    print("erorr on N,a or b!")
    f_x <- NULL
  }
  return(f_x)
}

#' Plot the Probability Density Functions
#'
#' Plot the p.d.f functions for the univariate distribution with data processed by StackRet.
#
#' @param data a tibble contains x, y and Variable and the last one is the group variable
#'
#' @return  A ggplot figure of the probability density functions
#' @export
#'
#' @examples
#' N <- 2**(1:6)
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' a <- -10.0
#' b <- 10.0
#' f_x1 <- CosPdfMulti(x, StNormChf, N, a, b)
#' colnames(f_x1) <- paste("N = 2 ^ ", c(1:6), sep = "")
#' mt1 <- StackRet(f_x1, x)
#' colnames(mt1) <- c("x", "y", "Variable")
#' PdfMultiPlot(mt1)
PdfMultiPlot <- function(data) {
  gg <- ggplot(data = data) +
    geom_line(mapping = aes(x = x, y = y, color = Variable)) +
    geom_point(mapping = aes(x = x, y = y, shape = Variable, color = Variable)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw()
  return(gg)
}

#' Calculate the Absolute Error of the COS Method
#'
#' Calculate the max absolute error of the cos method for different parameters given a vector of x.
#'
#' @param x vector of observations
#' @param f the true p.d.f.
#' @param Chf the characteristic function
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#' @param N the number of cos term for summation
#'
#' @return A matrix that contains the log max error for different parameters
#' @export
#'
#' @examples
#' N <- c(1:200)
#' L <- c(10, 20, 60, 100, 1000)
#' a <- -L / 2
#' b <- L / 2
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' LogErrorCosPdf(x, dnorm, NormChf, a, b, N)
LogErrorCosPdf <- function(x, f, Chf, a, b, N) {
  nn <- length(N)
  nl <- length(a)
  if (nl != length(b)){
    print("length of a donnot equals to length of b!")
    return(NULL)
  }
  error <- matrix(0, nrow = nn, ncol = nl)
  for (ii in seq_along(a)) {
    f_x2 <- CosPdfMulti(x, Chf, N, a[ii], b[ii])
    lerror <- f(x) - f_x2
    error[, ii] <- log(timeSeries::colMaxs(abs(lerror)))
  }
  return(error)
}

#' Rearrange the data from \code{LogErrorCosPdf} for plot
#'
#' @param error return of \code{LogErrorCosPdf}
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#' @param N the number of cos term for summation
#'
#' @return Suitable tibble data for plot by group in ggplot
#' @export
#'
#' @examples
#' N <- c(1:200)
#' L <- c(10, 20, 60, 100, 1000)
#' a <- -L / 2
#' b <- L / 2
#' x <- seq(-5, 5, by = 10 / (32 - 1))
#' el <- LogErrorCosPdf(x, dnorm, NormChf, a, b, N)
#' StackForPlot(el, a, b, N)
StackForPlot <- function(error, a, b, N) {
  if (length(N) > length(a)) {
    nameerror <- paste("L = ", b - a, sep = "")
    x <- N
  } else {
    nameerror <- paste("N = ", N, sep = "")
    error <- t(error)
    x <- b - a
  }
  colnames(error) <- nameerror
  te <- StackRet(error, x)
  colnames(te) <- c("x", "y", "Variable")
  return(te)
}

#' Calculate the V_k Series for Option Pricing with the COS Method
#'
#' @param ValueOption the value function of the option
#' @param N the number of cos term for summation
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#' @param method how to calculate the integral, one of "integrate" and "jiahe"
#'
#' @return The V_k series
#' @export
#'
#' @examples
#' r <- 0.1
#' sigmaS0 <- 0.2
#' tau <- 10
#' S0 <- 1
#' K <- 1
#' mu <- log(S0) + (r - 0.5 * sigmaS0^2) * tau
#' sigma <- sigmaS0 * sqrt(tau)
#' a <- -10
#' b <- 10
#' N <- 64
#' ValueOption <- function(x){EuroCallOption(x,K)}
#' V_k(ValueOption, N, a, b)
V_k <- function(ValueOption, N, a, b, method = "integrate") {
  k <- 0:(N - 1)
  vk <- function(kk) {
    uu <- kk * pi / (b - a)
    fk <- function(y) {
      ValueOption(y) * cos(uu * (y - a))
    }
    if (method == "integrate") {
      vkterm <- integrate(fk, a, b)$value * 2 / (b - a)
    } else {
      N_sim <- 10000
      y <- seq(a, b, (b - a) / N_sim)[-1]
      vkterm <- sum(fk(y)) * 2 / N_sim
    }
    return(vkterm)
  }
  V_k <- sapply(k, vk)
  return(V_k)
}

#' Approximate the Option Price with the COS Method
#'
#' Approximate the standard European call option price with the COS method.
#'
#' @param ValueOption the value function of the option
#' @param GBMChf the characteristic function for GBM
#' @param r the \code{r} parameter of GBM
#' @param tau the \code{tau} parameter of GBM
#' @param N the number of cos term for summation
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#' @param method how to calculate the integral, one of "integrate" and "jiahe"
#'
#' @return The approximated euro call option price
#' @export
#'
#' @examples
#' r <- 0.1
#' sigmaS0 <- 0.2
#' tau <- 10
#' S0 <- 1
#' K <- 1
#' mu <- log(S0) + (r - 0.5 * sigmaS0^2) * tau
#' sigma <- sigmaS0 * sqrt(tau)
#' a <- -10
#' b <- 10
#' N <- 64
#' GBMChf <- function(u){NormChf(u,mu,sigma)}
#' ValueOption <- function(x){EuroCallOption(x,K)}
#' CosValueOption(ValueOption, GBMChf,r,tau, N, a, b)
CosValueOption <- function(ValueOption, GBMChf, r, tau, N, a, b, method = "integrate") {
  F_K <- F_k(GBMChf, N, a, b)
  V_K <- V_k(ValueOption, N, a, b, method = method)
  Value <- exp(-r * tau) * sum(F_K * V_K) * (b - a) / 2
  return(Value)
}

#' The Value Function of European Call Option
#'
#' With global variable K, the strike price, calculate the value of European call option.
#'
#' @param x the stock price
#' @param K the strike price
#'
#' @return The value of European call option
#' @export
#'
#' @examples
#' EuroCallOption(x = 2,K = 1)
EuroCallOption <- function(x, K) {
  ecf <- function(x){
    return(max(exp(x) - K, 0))
  }
  value <- sapply(x, ecf)
  return(value)
}
