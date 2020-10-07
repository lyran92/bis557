#' @title Gradient descent based on the out-of-sample accuracy.
#' @description This is a function to fit the OLS model using gradient descent that calculates the penalty based on the out-of-sample accuracy.
#' @param formula The formula of the model to be fitted.
#' @param data_frame A data frame which contains the data for the model.
#' @param contrasts Default is NULL. A list of contrasts for factor variables.
#' @param lambda Default is 0.0001. The learning rate.
#' @param epsilon Default is 1e-20. The minimum difference between the current SSR and the updated SSR.
#' @param iters Default is 1e6. The maximum number of iterations.
#' @return A list of estimated coefficients.
#' @examples
#' data(iris)
#' model <- gradient_descent_new(Sepal.Length ~ ., data = iris)
#' model$coefficients
#' @export

gradient_descent_new <- function(formula, data_frame, contrasts = NULL, lambda=0.0001, epsilon=1e-20, iters=1e6){

  d_no_na <- model.frame(formula, data_frame)
  X <- model.matrix(formula,d_no_na, contrasts.arg = contrasts)
  y_name <- as.character(formula)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)

  set.seed(9999)
  train_ind <- sample(seq_len(nrow(X)), size = ceiling(0.7 * nrow(X)))

  X_train <- X[train_ind, ]
  X_test <- X[-train_ind, ]
  Y_train <- Y[train_ind]
  Y_test <- Y[-train_ind]

    if (qr(X_train)$rank==dim(X_train)[2]) {
      n <- length(Y_train)
      beta <- as.matrix(rep(1,ncol(X_train)))
      for(i in 1:iters){
        error <- sqrt(sum((X_test %*% beta- Y_test)^2))/n
        beta <-  beta-lambda*2*(t(X_train)%*% X_train %*% beta - t(X_train)%*%Y_train)
        error_new <- sqrt(sum((X_test %*% beta - Y_test)^2))/n
        if(abs(error_new-error) <epsilon){
          break
        }
      }
      result <- list(coefficients = beta)
      return(result)
    }else{
      warning("Gradient descent cannot be implemented in this case. Use linear_model function instead")
    }
}
