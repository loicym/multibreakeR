#' @title Sigma
#'
#' @description #compute the covariance matrix of errors as in Bai, Lumsdaine, and Stock (1998)
#'
#' @param mat.z A matrix of breaking and non breaking time series
#' @param mat.y.ex A vectorized matrix of time series
#' @param mat.beta The matrix of parameters
#' @param n.eq The number of equations in the VAR system
#'
#' @return The covariance matrix of errors
#' @export
#' @importFrom dplyr "%>%"



Sigma <- function(mat.z, mat.y.ex, mat.beta, n.eq) {
  #get the n*p vector of residuals
  errors <- (mat.y.ex - t(mat.z) %*% mat.beta)

  #reset as matrix to obtain mat.sigma
  mat.errors <-
    matrix(errors, ncol = n.eq, byrow = T)
  mat.sigma <- cov(mat.errors)

  return(mat.sigma)
}
