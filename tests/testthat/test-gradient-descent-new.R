library(testthat)

context("Test the new gradient descent function.")

test_that("Your gradient_descent_new() function works in an easy case.", {

  data(iris)

  fit_gradient_descent_new <- gradient_descent_new(Sepal.Length ~ Petal.Width, iris)

  fit_lm <- lm(Sepal.Length  ~ Petal.Width, iris)

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent_new $coefficients,
                    tolerance = 1e-1)
})
