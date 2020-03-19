#' @title The robust T2 test (T2R).
#' @description The robust version of the traditional T2 test using the comedian robust estimator.
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#'
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
#' T2Robust(X=X, mu0=mu0)
#'
#' @export
T2Robust <- function(X, mu0){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p  | (n<= p)) stop("The test cannot be performed.")
  robust <- covComed(X)
  Xs <- robust$raw.center
  Ss <- robust$raw.cov
  if (det(Ss)<= 0) stop("The covariance matrix must be positive definite.")
  T2 <- n * t(Xs - mu0) %*% solve(Ss) %*% (Xs - mu0)
  p.value <- 1 - pf((n - p) * T2 / ((n - 1) * p), p, n - p)
  return(list(T2 = T2, valor.p = p.value))
}
