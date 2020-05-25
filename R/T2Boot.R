#' @title The parametric bootstrap T2 test (T2Boot).
#' @description The parametric bootstrap version of the traditional T2 test.
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#' @param B the number of resamples bootstrap parametric which must be at least equal to 2000.
#'
#'
#' @importFrom MASS mvrnorm ginv
#' @importFrom stats var pf
#'
#' @return the numerical value and the p-value of the test statistic.
#'
#' @references
#' Henrique J. P. Alves & Daniel F. Ferreira (2019): Proposition of new alternative tests adapted to the traditional T2 test, Communications in Statistics - Simulation and Computation, DOI: 10.1080/03610918.2019.1693596
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
#' B=2000
#' X <- mvrnorm(n, mu, Sigma)
#' T2Boot(X=X, mu0=mu0, B=2000)
#'
#' @export
T2Boot <- function(X, mu0, B)
{
  n <- nrow(X)
  p <- ncol(X)
  XS <- apply(X,2,mean)
  SS <- var(X)
  T2 <- n * t(XS - mu0) %*% ginv(SS) %*% (XS - mu0)
  T2v <- T2
  for (i in 1:B)
  {
    Xb <- mvrnorm(n, mu0, SS)
    Xsb <- apply(Xb,2,mean)
    Ssb <- var(Xb)
    T2b <- n * t(Xsb - mu0) %*% ginv(Ssb) %*% (Xsb - mu0)
    T2v <- c(T2v,T2b)
  }
  p.value <- length(T2v[as.numeric(T2) <= T2v]) / (B + 1)
  return(list(T2 = T2, valor.p = p.value))
}
