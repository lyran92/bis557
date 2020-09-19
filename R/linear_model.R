#' @title Fit a linear model.
#' @description This a function to fit the linear model.
#' @param formula The formula of the regression model to be fitted.
#' @param data_frame A data frame which contains the data for the model.
#' @param contrasts A list of contrasts for factor variables.
#' @return A list of estimated coefficients.
#' @examples
#' data(iris)
#' model <- linear_model(Sepal.Length ~., iris, contrasts = list(Species = "contr.sum"))
#' model$coefficients
#' @export

linear_model <- function(formula, data_frame, contrasts = NULL){
  
  d_no_na <- model.frame(formula, data_frame)
  
  X <- model.matrix(formula,d_no_na, contrasts.arg = contrasts)
  
  y_name <- as.character(formula)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)
  
  beta <- qr.solve(qr(X), Y)
  beta[beta == 0] <- NA
  beta_names <- rownames(beta)
  beta <- as.numeric(beta)
  names(beta) <- beta_names
  result <- list(coefficients = beta)
  class(result) <- "my_lm"
  return(result)
}
