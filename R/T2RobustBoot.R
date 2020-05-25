#' @title The T2 robust parametric bootstrap test (T2RPB).
#' @description The robust parametric bootstrap version of the traditional T2 test using the comedian robust estimator.
#'
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#' @param B the number of resamples bootstrap parametric which must be at least equal to 2000.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats var pf
#' @importFrom robustbase covComed
#'
#' @return the numerical value and the p-value of the test statistic.
#'
#' @references
#' Henrique J. P. Alves & Daniel F. Ferreira (2019): Proposition of new alternative tests adapted to the traditional T2 test, Communications in Statistics - Simulation and Computation, DOI: 10.1080/03610918.2019.1693596
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
#' T2RobustBoot(X=X, mu0=mu0, B=B)
#'
#' @export
T2RobustBoot <- function(X, mu0, B){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p  | (n<= p)) stop("The test cannot be performed.")
  robust <- covComed(X)
  Xs <- robust$raw.center
  Ss <- robust$raw.cov
  if (det(Ss)<= 0) stop("The covariance matrix must be positive definite.")
  T2 <- n * t(Xs - mu0) %*% ginv(Ss) %*% (Xs - mu0)
  T2v <- T2
  for (i in 1:B)
  {
    Xb <- mvrnorm(n, mu0, Ss)
    robust <- covComed(Xb)
    Xsb <- robust$raw.center
    Ssb <- robust$raw.cov
    T2b <- n * t(Xsb - mu0) %*% ginv(Ssb) %*% (Xsb - mu0)
    T2v <- c(T2v,T2b)
  }
  p.value <- length(T2v[as.numeric(T2) <= T2v]) / (B + 1)
  return(list(T2 = T2, valor.p = p.value))
}

