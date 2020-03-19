#' @title The robust likelihood ratio test parametric bootstrap with trace test (RTLRPBT).
#' @description An robust alternative test version of the likelihood ratio test (LRT) parametric bootstrap with trace (RTLPBT) to test the hypotheses about a vector of population averages using the comedian robust estimator. This test has the advantage of being valid for high dimension data (n <p)
#'
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#' @param B the number of resamples bootstrap parametric which must be at least equal to 2000.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats var pf pchisq
#' @importFrom robustbase covComed
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
#' B <- 200
#' X <- mvrnorm(n, mu, Sigma)
#' LRTTRBoot(X=X, mu0=mu0, B=B)
#'
#' @export
LRTTRBoot <- function(X, mu0, B){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p) stop("The test cannot be performed.")
  robust <- covComed(X)
  Xsrb <- robust$raw.center
  Ssrb <- robust$raw.cov
  H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
  LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
  LRTTv <- LRTTo
  for (i in 1:B)
  {
    Xrbb <- mvrnorm(n, mu0, Ssrb)
    robust <- covComed(Xrbb)
    Xsrbb <- robust$raw.center
    Ssrbb <- robust$raw.cov
    H <- (Xsrbb - mu0) %*% t(Xsrbb - mu0)
    LRTTb <- n * (log(sum(diag(Ssrbb + H))) - log(sum(diag(Ssrbb))))
    LRTTv <- c(LRTTv, LRTTb)
  }
  p.value <- length(LRTTv[as.numeric(LRTTo) <= LRTTv]) / (B + 1)
  return(list(LRTT = LRTTo, valor.p = p.value))
}
