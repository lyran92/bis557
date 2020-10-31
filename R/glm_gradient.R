#' @title GLM based on gradient.
#' @description A first-order solution for the GLM maximum likelihood problem using gradient information,including both a constant step size along with an adaptive one.
#' @param X The input design matrix.
#' @param y The input response vector.
#' @param family The error distribution and link function.
#' @param step An option to choose whether use a constant step size or an adaptive one,momentum.
#' @param lambda A number indicating the learning rate.
#' @param gamma A fraction indicating the momentum step size.
#' @param maxiter A number indicating the maximum number iterations.
#' @param tol A number indicating the covergence of the algorithm.
#' @return A list of estimated coefficients.
#' @examples
#'set.seed(999)
#'n <- 1000; p <- 5;
#'X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
#'beta <- c(-1, 0.3, 2, 0.1, 0.5)
#'Y <- rpois(n, lambda = exp(X %*% beta))
#'glm_gradient(X,Y,family=poisson(link = "log"),step = "constant")
#'glm_gradient(X,Y,family=poisson(link = "log"),step = "momentum")
#' @export

glm_gradient <-function(X, y,family,step = c("constant", "momentum"),lambda = 1e-5,gamma =0.8,maxiter = 1e3,tol = 1e-12){

  beta <- rep(0, ncol(X)) #Initialize the parameters
  for(i in seq_len(maxiter)){
    beta_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    grad <- t(X) %*% (y - mu) #The derivation of beta, it is the first order

    if(step == "constant"){
      beta <- beta + lambda * grad
    }
    if(step == "momentum"){
      grad_new <- gamma*beta+(1-gamma)* grad #Update the gradient of beat and the current beta itself by weighted averaging them.
      beta <- beta+lambda*grad_new
    }
    #I refer the concept from the coursera course "Gradient descent with momentum" by Andrew Ng.

    if(sqrt(crossprod(beta - beta_old)) < tol) break
  }

  result <- list(coefficients = beta)
  return(result)
}



