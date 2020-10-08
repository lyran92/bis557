#' @title Ridge Regression
#' @description This is a function to implement a ridge regression function taking into account colinear (or nearly colinear) regression variables.
#' @param X The input design matrix.
#' @param Y The input response vector.
#' @param lambda The input penalty parameter.
#' @examples
#' model <- ridge_regression(X, Y, lambda=0.5)
#' model$coefficients
#' @export

ridge_regression <- function(X, Y, lambda){
  svd_x <- svd(X)
  U <- svd_x$u
  V <- svd_x$v
  sigma <- svd_x$d
  beta <- V %*% diag(sigma/(sigma^2+lambda)) %*% t(U) %*% Y
  result <- list(coefficients = beta, lambda = lambda)
  return(result)
}

