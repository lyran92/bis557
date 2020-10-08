library(testthat)
library(MASS)

context("Test the optimize_ridge function.")

test_that("You optimize_ridge() function works well", {

  n <- 500
  p <- 5
  beta <- c(1,-1,0,0,2)
  set.seed(9999)
  X <- matrix(rnorm(n * p), ncol = p)
  alpha <- 0.05
  X[,1] <- X[,1] * alpha + X[,2] * (1 - alpha)
  Y <- X %*% beta + rnorm(n)
  lambdas <- seq(0.1,10, by=0.1)

  data <-cbind(y,X)
  data <- as.data.frame(data)
  fit_lm <- lm.ridge(V1~.-1,data,lambda =lambdas)
  lambda_lm=lambdas[fit_lm$GCV==min(fit_lm$GCV)]
  lambda_ridge = optimize_ridge(X,Y,lambdas,k=5)

  expect_equivalent(lambda_lm, lambda_ridge,
                    tolerance = 0.1)
})
