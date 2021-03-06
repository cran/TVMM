% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LRTTrace.R
\name{LRTTrace}
\alias{LRTTrace}
\title{The likelihood ratio test with trace (TLRT)}
\usage{
LRTTrace(X, mu0)
}
\arguments{
\item{X}{a matrix n x p containing n observations and p variables. It should not contain missing values (NA).}

\item{mu0}{a vector containing the mean population to be tested.}
}
\value{
the numerical value and the p-value of the test statistic.
}
\description{
An asymptotic version of the Likelihood ratio test (LRT) to test the hypotheses about a vector of population averages. This test has the advantage of being valid for high dimension data (n < p).
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
LRTTrace(X=X, mu0=mu0)

}
