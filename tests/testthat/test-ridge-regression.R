library(testthat)

context("Test the ridge_regression function.")

test_that("You ridge_regression() function works well", {

  n <- 1000
  p <- 5
  beta <- c(1,-1,0,0,2)
  set.seed(2)
  X <- matrix(rnorm(n*p), nrow=n, ncol = p)
  alpha <- 0.05
  X[,1] <- X[,1] * alpha + X[,2] * (1 - alpha)
  set.seed(1)
  Y <- X %*% beta + rnorm(n)
  fit_ridge_regression = ridge_regression(X,Y,1.2)

  expect_equivalent(beta, fit_ridge_regression$coefficients,
                    tolerance = 0.2)
})


