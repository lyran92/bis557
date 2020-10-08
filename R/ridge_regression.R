#' @title Ridge Regression
#' @description This is a function to implement a ridge regression function taking into account colinear (or nearly colinear) regression variables.
#' @param X The input design matrix.
#' @param Y The input response vector.
#' @param lambda The input penalty parameter.
#' @return A list of estimated coefficients.
#' @examples
#' n <- 1000
#' p <- 5
#' beta <- c(1,-1,0,0,2)
#' set.seed(2)
#' X <- matrix(rnorm(n*p), nrow=n, ncol = p)
#' alpha <- 0.05
#' X[,1] <- X[,1]*alpha+X[,2]*(1 - alpha)
#' set.seed(1)
#' Y <- X %*% beta + rnorm(n)
#' model <- ridge_regression(X, Y, lambda=0.5)
#' model$coefficients
#' @export

ridge_regression <- function(X, Y, lambda){
  svd_x <- svd(X)
  U <- svd_x$u
  V <- svd_x$v
  sigma <- svd_x$d
  beta <- V %*% diag(sigma/(sigma^2+lambda)) %*% t(U) %*% Y
  result <- list(coefficients = beta)
  return(result)
}

