#' @title Fitting a linear model using Python.
#' @description This is a function fits linear model.
#' @param X The input design matrix.
#' @param y The input response vector.
#' @return A list of estimated coefficients.
#' @examples
#' \dontrun{pylinear(X,y)}
#' @export

pylinear <- function(X,y){
  qrs <- r_to_py(np$linalg$qr(X))
  q <- qrs[[0]]
  r <- qrs[[1]]
  beta <- py_to_r(np$linalg$inv(r)$dot(q$T)$dot(y))

  result <- list(coefficients = beta)
  return(result)
}
