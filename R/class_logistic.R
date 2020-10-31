#' @title Multi-Class Logistic Regression
#' @description A classification model generalizing logistic regression to accommodate more than two classes.
#' @param X The input design matrix.
#' @param y The input response vector.
#' @param maxiter A number indicating the maximum number iterations.
#' @return A list of estimated coefficients and the classification of the observations.
#' @examples
#'data(iris)
#'iris1 <- iris[sample(150,replace = FALSE),]
#'iris.X <- as.matrix(iris1[,-5])
#'iris.y <- iris1$Species
#'classification <- class_logistic(iris.X, iris.y, maxiter=50)
#' @export

class_logistic <- function(X, y, maxiter) {
  k <- length(unique(y)) #The number of classes
  #Initialize the parameters
  beta <- matrix(0, nrow = k, ncol = ncol(X))
  prob <- matrix(0, nrow = k, ncol = nrow(X))


  for (i in 1:k) {
    y_new <- ifelse(y==unique(y)[i], 1, 0) #If y belongs to the ith class, then y_new equals to 1.
    for (j in 1:maxiter) {
      p <- 1 / (1 + exp(-X %*% beta[i,]))
      D <- diag(as.numeric(p * (1 - p)))#Variance matrix of probabilities
      Hessian <- t(X) %*% D %*% X
      grad <- t(X) %*% (y_new - p)
      beta[i,] <- beta[i,] + solve(Hessian) %*% grad
    }
  }

  #This step doesn't have any practical meaning.
  #Just to find a place to store each value in order to calculate their sum.
  for (i in 1:k) {
    prob[i,] <- exp(X%*%beta[i,])
  }
  sum <- sum(prob)
  #We use softmax function to get the probablity of each observation belongs to three classes.
  for (i in 1:k) {
    prob[i,] <- exp(X%*%beta[i,])/sum
  }

  #Get the classification (prediction) by assigning the obersvation to the class with the highest probability.
  y_pre <- unique(y)[apply(prob, 2, which.max)]

  result <- list(coefficients = beta, prediction = y_pre)

return(result)
}


