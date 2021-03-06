% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_gradient.R
\name{glm_gradient}
\alias{glm_gradient}
\title{GLM based on gradient.}
\usage{
glm_gradient(
  X,
  y,
  family,
  step = c("constant", "momentum"),
  lambda = 1e-05,
  gamma = 0.8,
  maxiter = 1000,
  tol = 1e-12
)
}
\arguments{
\item{X}{The input design matrix.}

\item{y}{The input response vector.}

\item{family}{The error distribution and link function.}

\item{step}{An option to choose whether use a constant step size or an adaptive one,momentum.}

\item{lambda}{A number indicating the learning rate.}

\item{gamma}{A fraction indicating the momentum step size.}

\item{maxiter}{A number indicating the maximum number iterations.}

\item{tol}{A number indicating the covergence of the algorithm.}
}
\value{
A list of estimated coefficients.
}
\description{
A first-order solution for the GLM maximum likelihood problem using gradient information,including both a constant step size along with an adaptive one.
}
\examples{
set.seed(999)
n <- 1000; p <- 5;
X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
beta <- c(-1, 0.3, 2, 0.1, 0.5)
Y <- rpois(n, lambda = exp(X \%*\% beta))
glm_gradient(X,Y,family=poisson(link = "log"),step = "constant")
glm_gradient(X,Y,family=poisson(link = "log"),step = "momentum")
}
