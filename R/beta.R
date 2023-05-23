#' @title Beta
#'
#' @description #Compute the matrix of parameters and the covariance
#' matrix of errors in OLS, FGLS, or IGLS mode.
#'
#' @param mat.z A matrix object of time series, regressor matrix
#' @param mat.y.ex A matrix object of time series, regressor matrix
#' @param n.eq number of equations in the VAR
#' @param p number of observations
#' @param est.mode estimation mode: "OLS", "FGLS", or "IGLS"
#' @param iter If "IGLS" is used, how many iterations before stopping
#' @return A list with the matrix of beta parameters as first element
#' and the covariance matrix of error as second element.
#' @export
#' @importFrom dplyr "%>%"

Beta <- function(mat.z,
                 mat.y.ex,
                 n.eq,
                 p,
                 est.mode,
                 iter) {
  #if OLS
  if (est.mode == "OLS") {
    #solve the system and get the vector of betas
    mat.beta <-
      solve(mat.z %*% t(mat.z), tol = 0) %*% mat.z %*% mat.y.ex
  }

  #if FGLS
  if (est.mode == "FGLS") {
    #solve the system and get the vector of betas
    mat.beta <-
      solve(mat.z %*% t(mat.z), tol = 0) %*% mat.z %*% mat.y.ex
    #get the covariance matrix of errors
    mat.sigma <-
      Sigma(mat.z, mat.y.ex, mat.beta, n.eq)
    #get Omega
    mat.omega <-
      kronecker(diag(p), mat.sigma)
    mat.beta <-
      solve(mat.z %*% solve(mat.omega, tol = 0) %*% t(mat.z), tol = 0) %*% mat.z %*% solve(mat.omega, tol = 0) %*% mat.y.ex        #solve the system and get the vector of betas
  }

  #if IGLS
  if (est.mode == "IGLS") {
    #solve the system and get the vector of betas
    mat.beta <-
      solve(mat.z %*% t(mat.z), tol = 0) %*% mat.z %*% mat.y.ex

    for (i in 1:iter) {
      #get the covariance matrix of errors
      mat.sigma <- Sigma(mat.z, mat.y.ex, mat.beta, n.eq)
      #get Omega
      mat.omega <-
        kronecker(diag(p), mat.sigma)
      mat.beta <-
        solve(mat.z %*% solve(mat.omega, tol = 0) %*% t(mat.z), tol = 0) %*% mat.z %*% solve(mat.omega, tol = 0) %*% mat.y.ex      #solve the system and get the vector of betas
    }
  }
  #final estimation of Sigma
  mat.sigma <-
    Sigma(mat.z, mat.y.ex, mat.beta, n.eq)
  return(list(mat.beta = mat.beta, mat.sigma = mat.sigma))
}
