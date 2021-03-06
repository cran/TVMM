% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T2O.R
\name{T2O}
\alias{T2O}
\title{The traditional T2 test (T2).}
\usage{
T2O(X, mu0)
}
\arguments{
\item{X}{a matrix n x p containing n observations and p variables. It should not contain missing values (NA).}

\item{mu0}{a vector containing the mean population to be tested.}
}
\value{
the numerical value and the p-value of the test statistic.
}
\description{
The traditional T2 test (T2).
}
\examples{
set.seed(0)
library(MASS)
n <- 30
p <- 2
rho <- 0.9
delta <- 0.9
mu <- rep(0, times  = p)
Sigma <- (1 - rho) * diag(p) + rho * matrix(1, p, p)
mu0 <- rep(0.3271,times = p)
X <- mvrnorm(n, mu, Sigma)
T2O(X=X, mu0=mu0)

}
\references{
Henrique J. P. Alves & Daniel F. Ferreira (2019): Proposition of new alternative tests adapted to the traditional T2 test, Communications in Statistics - Simulation and Computation, DOI: 10.1080/03610918.2019.1693596
}
