#' @title Lags
#'
#' @description Compute the lags for the mat.y time series matrix
#'
#' @param mat.y The matrix of time series
#' @param q The lag chosen
#'
#' @return A list of original (dependent) and lagged (independent) time series matrix
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' data(example_data)
#' list.lags <- Lags(mat.y = example_data, q = 2)


Lags <- function(mat.y,
                 q) {
  #get the dimensions
  p <-
    dim(mat.y)[1]


  n <- dim(mat.y)[2]

  #optional: keep the rownames dates of the data frame with final matching
  my.dates <-
    rownames(mat.y)[(q + 1):p]

  #matrix conversion
  mat.y <-
    data.matrix(mat.y)

  #create an empty matrix
  mat.y.lag <-
    matrix(data = NA, nrow =
             (p - q), ncol =
             (n * (q + 1)))

  for (i in 0:q) {
    mat.y.lag[, (n * i + 1):(n * (i + 1))] <-
      mat.y[(q - i + 1):(p - i),]
  }

  mat.y <- data.matrix(mat.y.lag[, 1:n])
  mat.y.lag <- data.matrix(mat.y.lag[, (n + 1):dim(mat.y.lag)[2]])
  return(list(
    mat.y = mat.y,
    mat.y.lag = mat.y.lag,
    my.dates = my.dates
  ))
}
