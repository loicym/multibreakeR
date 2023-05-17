#' @title ConfidenceInterval
#'
#' @description Compute the confidence interval in time unit.
#'
#' @param mat.g A matrix object of time series
#' @param mat.s A selection matrix
#' @param mat.sigma The covariance matrix
#' @param mat.r The selection vector of parameters
#' @param mat.beta The matrix of parameters
#' @param cv A vector of critical values
#' @param p The length of the vector
#' @return The difference in time unit around the break
#' @export
#' @importFrom dplyr "%>%"

ConfidenceInterval <-
  function(mat.g,
           mat.s,
           mat.sigma,
           mat.r,
           mat.beta,
           cv,
           p) {
    #get the number of critical values from the vdistr
    n <- length(cv)

    #create empty vector
    ci.delta <- rep(NA, n)

    #pre compute the selection of (breaking) parameters to test
    mat.r.beta <- mat.r %*% mat.beta

    #compute the confidence interval factor
    t.ci <-
      solve(t(mat.r.beta) %*% mat.s %*% kronecker((mat.g %*% t(mat.g)) / p, solve(mat.sigma, tol = 0)) %*% t(mat.s) %*% mat.r.beta,
            tol = 0)

    for (i in 1:n) {
      #get the vector of critical values
      ci.delta[i] <- cv[i] * t.ci
    }
    return(ci.delta)
  }
