#' @title Fstat
#'
#' @description Compute the f-statistic for the break test
#'
#' @param mat.r The selection matrix for the parameters
#' @param mat.beta The matrix of parameters
#' @param mat.z The matrix of original and "breaking" time series
#' @param p The number of observations
#' @param mat.sigma The covariance matrix
#' @return The f-statistic scalar
#' @export
#' @importFrom dplyr "%>%"

Fstat <- function(mat.r,
                  mat.beta,
                  mat.z, p,
                  mat.sigma) {
  #pre compute the mat.r.beta matrix with the selected coefficients allowed to break
  mat.r.beta <- mat.r %*% mat.beta

  #if the covariance matrix of error is passed as argument, compute F-stat
  if (!is.null(mat.sigma)) {
    #get Omega
    mat.omega <- kronecker(diag(p), mat.sigma)
    f.k <-
      p * t(mat.r.beta) %*% solve(mat.r %*% solve((
        mat.z %*% solve(mat.omega, tol = 0) %*% t(mat.z)
      ) / p, tol = 0) %*% t(mat.r), tol = 0) %*% mat.r.beta
  }
  return(f.k)
}
