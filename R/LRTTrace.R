#' @title The likelihood ratio test with trace (TLRT)
#' @description An asymptotic version of the Likelihood ratio test (LRT) to test the hypotheses about a vector of population averages. This test has the advantage of being valid for high dimension data (n < p).
#'
#'
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats var pf pchisq
#'
#' @return the numerical value and the p-value of the test statistic.
#'
#'
#' @examples
#' set.seed(0)
#' library(MASS)
#' n <- 30
#' p <- 2
#' rho <- 0.9
#' delta <- 0.9
#' mu <- rep(0, times  = p)
#' Sigma <- (1 - rho) * diag(p) + rho * matrix(1, p, p)
#' mu0 <- rep(0.3271,times = p)
#' X <- mvrnorm(n, mu, Sigma)
#' LRTTrace(X=X, mu0=mu0)
#'
#' @export
LRTTrace <- function(X, mu0){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p) stop("The test cannot be performed.")
  S <- var(X)
  Xb <- apply(X, 2, mean)
  H <- (Xb - mu0) %*% t(Xb - mu0)
  LRTT <- n * (log(sum(diag(S + H))) - log(sum(diag(S))))
  p.value <- 1 - pchisq(LRTT,p)
  return(list(LRTT = LRTT, valor.p = p.value))
}

