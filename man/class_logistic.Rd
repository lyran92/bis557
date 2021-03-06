% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_logistic.R
\name{class_logistic}
\alias{class_logistic}
\title{Multi-Class Logistic Regression}
\usage{
class_logistic(X, y, maxiter)
}
\arguments{
\item{X}{The input design matrix.}

\item{y}{The input response vector.}

\item{maxiter}{A number indicating the maximum number iterations.}
}
\value{
A list of estimated coefficients and the classification of the observations.
}
\description{
A classification model generalizing logistic regression to accommodate more than two classes.
}
\examples{
data(iris)
iris1 <- iris[sample(150,replace = FALSE),]
iris.X <- as.matrix(iris1[,-5])
iris.y <- iris1$Species
classification <- class_logistic(iris.X, iris.y, maxiter=50)
}
