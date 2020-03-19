#' @title The likelihood ratio parametric bootstrap with trace test (TLRPBT). This test is more powerful.
#' @description An parametric bootstrap alternative test version of the Likelihood ratio test (LRT) to test the hypotheses about a vector of population averages. This test has the advantage of being valid for high dimension data (n <p). This test should be preferred by the user, as it controlled the type I error and had greater power in all scenarios evaluated.
#'
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#' @param B the number of resamples bootstrap parametric which must be at least equal to 2000.
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
#' B <- 200
#' X <- mvrnorm(n, mu, Sigma)
#' LRTTBoot(X=X, mu0=mu0, B=B)
#'
#' @export
LRTTBoot <- function(X, mu0, B){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p) stop("The test cannot be performed.")
  Xsrb <- apply(X, 2, mean)
  Ssrb <- var(X)
  H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
  LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
  LRTTv <- LRTTo
  for (i in 1:B)
  {
    Xbb <- mvrnorm(n, mu0, Ssrb)
    Xsbb <- apply(Xbb, 2, mean)
    Ssbb <- var(Xbb)
    H <- (Xsbb - mu0) %*% t(Xsbb - mu0)
    LRTTb <- n * (log(sum(diag(Ssbb + H))) - log(sum(diag(Ssbb))))
    LRTTv <- c(LRTTv, LRTTb)
  }
  p.value <- length(LRTTv[as.numeric(LRTTo) <= LRTTv]) / (B + 1)
  return(list(LRTT = LRTTo, valor.p = p.value))
}

