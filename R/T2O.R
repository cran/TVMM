#' @title The traditional T2 test (T2).
#' @description The traditional T2 test (T2).
#'
#' @param X a matrix n x p containing n observations and p variables. It should not contain missing values (NA).
#' @param mu0 a vector containing the mean population to be tested.
#'
#' @importFrom MASS mvrnorm
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
#' X <- mvrnorm(n, mu, Sigma)
#' T2O(X=X, mu0=mu0)
#'
#' @export
T2O <- function(X, mu0){
  n <- nrow(X)
  p <- ncol(X)
  if (length(mu0)!= p  | (n<= p)) stop("The test cannot be performed.")
  Xs <- apply(X,2,mean)
  Ss <- var(X)
  if (det(Ss)<= 0) stop("The covariance matrix must be positive definite.")
  T2 <- n * t(Xs - mu0) %*% solve(Ss) %*% (Xs - mu0)
  p.value <- 1 - pf((n - p) * T2 / ((n - 1) * p), p, n - p)
  return(list(T2 = T2, valor.p = p.value))
}
