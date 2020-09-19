#' @title Gradient descent for ordinary least squares.
#' @description This is a function to implement gradient descent for ordinary least squares.
#' @param formula The formula of the model to be fitted.
#' @param data_frame A data frame which contains the data for the model.
#' @param contrasts Default is NULL. A list of contrasts for factor variables.
#' @param lambda Default is 0.0001. The learning rate.
#' @param epsilon Default is 1e-20. The minimum difference between the current SSR and the updated SSR.
#' @param iters Default is 1e6. The maximum number of iterations.
#' @return A list of estimated coefficients.
#' @examples
#' data(iris)
#' model <- grdient_descent(Sepal.Length ~ ., data = iris)
#' model$coefficients
#' @export

grdient_descent <- function(formula, data_frame, contrasts = NULL, lambda=0.0001, epsilon=1e-20, iters=1e6){
  
  d_no_na <- model.frame(formula, data_frame)
  X <- model.matrix(formula,d_no_na, contrasts.arg = contrasts)
  y_name <- as.character(formula)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)

  if (qr(X)$rank==dim(X)[2]) {
    beta <- as.matrix(rep(1,ncol(X)))
    error <- sum((X %*% beta- Y)^2)
    for(i in 1:iters){
      error <- sum((X %*% beta- Y)^2)
      beta <-  beta-lambda*2*(t(X)%*% X %*% beta - t(X)%*%Y)
      error_new <- sum((X %*% beta - Y)^2)
      if(abs(error_new-error) <epsilon){
        break
      }
    }
    result <- list(coefficients = beta)
    return(result)
  }else{
    warning("Gradient descent cannot be implemented in this case. Use linear_model function instead")
    result  <- linear_model(formula, data_frame, contrasts)
    return(result)
  }
  
}
