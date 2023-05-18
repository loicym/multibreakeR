#' @title Vdistr
#'
#' @description Computes the critical values for a vector of confidence intervals proposed (ci)
#'
#' @param ci A vector of confidence intervals
#'
#' @return A vector of critical values
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats pnorm


Vdistr <- function(ci) {
  #get the number of confidence interval elements
  n <- length(ci)
  #redefine target for a two tail confidence interval
  target <-
    1 - (1 - ci) / 2

  print(paste0("The vdistr targets are: ", target))
  #define the support sequence "x" for the CDF of V
  x <-
    seq(-200, 200, 0.01)

  #compute mat.v
  mat.v <-
    (3 / 2) * exp(abs(x)) * pnorm((-3 / 2) * abs(x) ^ 0.5)  - (1 / 2) * pnorm((-1 / 2) * abs(x) ^
                                                                                0.5)

  #scale the CDF of mat.v to reach one
  cum.v <-
    cumsum(mat.v) / sum(mat.v)

  #optionally plot v
  # dev.new()
  # plot(x, cumsum(gamma)/sum(gamma), t = 'l')

  cv <- rep(NA, n)
  k <- 1

  print(target)
  for (i in 2:length(x)) {
    if (cum.v[i - 1] < target[k] && cum.v[i] >= target[k]) {
      cv[k] <- x[i]
      k <- k + 1
      if (k > n)
        break
    }
  }
  return(cv)
}
