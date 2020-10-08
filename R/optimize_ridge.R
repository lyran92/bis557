#' @title Optimizing the ridge parameter
#' @description This is a function to optimize the ridge penalty parameter.
#' @param X The input design matrix.
#' @param Y The input response vector.
#' @param lambdas The list of input penalty parameters.
#' @param k An optional number k for k-fold cross-validation.
#' @examples
#' result <- optimize_ridge(X, Y, lambda=seq(0.1,15,length.out=50),k=5)
#' print(result)
#' @export

optimize_ridge <- function(X,Y,lambdas,k){

  fold.index <- sample(rep(1:k,length.out =nrow(X)))
  X<-as.data.frame(X)
  Y<-as.data.frame(Y)
  X$fold <- fold.index
  Y$fold <- fold.index
  cv_error <- matrix(0,nrow = length(lambdas),ncol = k)
  cv_mse <- rep(0, length(lambdas))

  for (i in 1:length(lambdas)){
   for (j in 1:k){
    X_test  <- X[X$fold==j,]
    X_test  <- X_test[,-ncol(X_test)]
    X_train <- X[X$fold!=j,]
    X_train <- X_train[,-ncol(X_train)]

    Y_test  <- Y[Y$fold==j,]
    Y_test  <- Y_test[,-ncol(Y_test)]
    Y_train <- Y[Y$fold!=j,]
    Y_train <- Y_train[,-ncol(Y_train)]

    fit <- ridge_regression(X_train,Y_train,lambdas[i])
    beta_est <- fit$coefficients
    X_test <- as.matrix(X_test)
    Y_pre <- X_test %*% beta_est
    cv_error[i,j] <- apply((Y_pre-Y_test)^2, 2, mean)
   }
    cv_mse[i] <- mean(cv_error[i,])
  }
  return(lambdas[cv_mse == min(cv_mse)])
}

