#' @title AicBic
#'
#' @description Compute the AIC and BIC criteria for lags from 1 to q.max
#'
#' @param mat.y A matrix object of time series
#' @param q.max The maximum lag considered
#' @param mat.x An optional matrix of covariates
#' @param trend If a trend is considered (default to false)
#' @param intercept If the test is on the intercept (default to true)
#'
#' @return A data frame object that contains all AIC (first row) and BIC (second row) for all the q.max lags tested.
#' @export
#' @importFrom dplyr "%>%"



AicBic <-
  function(mat.y,
           q.max,
           mat.x,

           trend = FALSE,
           intercept = TRUE) {
    #create empty matrix for the AIC/BIC criteria

    aic.bic <-
      matrix(data <-
               NA, nrow = 2, ncol = q.max)

    for (q in 1:q.max) {
      print(paste0("Testing lags number : ", q))

      #create a list of conformed objects for the estimation
      l.conf.matrix <-
        ConformableMatrix(mat.y, q, mat.x, trend, intercept)
      mat.y.ex <- l.conf.matrix$mat.y.ex
      mat.g.ex <- l.conf.matrix$mat.g.ex

      #estimate the model with lm
      mod <-
        lm(mat.y.ex ~ mat.g.ex)
      #get AIC
      aic.bic[1, q] <-
        AIC(mod)
      #get BIC
      aic.bic[2, q] <-
        BIC(mod)
    }

    rownames(aic.bic) <- c("AIC", "BIC")
    colnames(aic.bic) <- paste0("lags = ", 1:q.max)
    return(aic.bic)
  }
