#' @title ConformableMatrix
#'
#' @description Compute the list of matrices with correct dimensions to pass later in the computation
#'
#' @param mat.y The matrix object of time series
#' @param q The chosen lag
#' @param mat.x The matrix of optional covariates
#' @param trend Whether we add a trend. Default = FALSE
#' @param intercept Whether the break test is on the intercept only. Default = TRUE
#' @return A list of conformed matrices
#' @export
#' @importFrom dplyr "%>%"



ConformableMatrix <- function(mat.y, q, mat.x, trend = FALSE, intercept = TRUE) {

  #get the original number of observations
  p.init <-
    dim(mat.y)[1]

  #get the list of contemporaneous and lags objects
  l.y <-
    Lags(mat.y = mat.y, q = q)

  #get the matching dependent matrix
  mat.y <- l.y$mat.y
  #get the lagged dependent variables matrix
  mat.y.lag <- l.y$mat.y.lag
  #get the original matching rowname vector (of dates)
  my.dates <-
    l.y$my.dates

  #length of the matrix
  p <- dim(mat.y)[1]
  #number of equations of the VAR
  n.eq <-
    dim(mat.y)[2]

  print(p)

  #identity matrix of the number of equations of the VAR
  id.n <-
    diag(n.eq)

  #create a unique matrix transpose of G with one intercept and autoregressive terms
  mat.g.t <-
    as.matrix(cbind(rep(1, p), mat.y.lag))

  #incremental number of regressors
  n <-
    dim(mat.g.t)[2]

  #if additional covariates matrix is passed as argument
  if (!is.null(mat.x)) {
    #and if its size is equal to the original matrix
    if (p.init == dim(mat.x)[1]) {
      #increment mat.g.t by the contemporaneous covariate matrix mat.x
      mat.g.t <-
        cbind(mat.g.t, data.matrix(mat.x[(q + 1):p.init,]))
      #increment the total number of regressors
      n <-
        dim(mat.g.t)[2]
    }

    else
      print("The number of observations of X does not match the one of Y")
  }

  #check if we add a trend
  if (trend) {
    #add trend
    mat.g.t <- cbind(mat.g.t, seq(1, p, by = 1))
    #increment the total number of regressors
    n <-
      dim(mat.g.t)[2]
  }

  #if only the intercept is allowed to break
  if (intercept) {
    #create the selection vector
    s <- t(data.matrix(c(rep(0, n))))
    #vector only select the first element (intercept) to test the shift
    s[1] <- 1
    #create the selection matrix
    mat.s <-
      kronecker(s, id.n)
  }

  #full parameters structural change estimation
  if (!intercept) {
    #get the full dimension of the test
    r <- n.eq * n
    #identity matrix of the size of the test is the selection matrix
    mat.s <-
      diag(r)
  }

  #transpose mat.g.t to get mat.g as in BLS
  mat.g <-
    t(mat.g.t)

  #Expand and vectorize mat.y
  mat.y.ex <-
    data.matrix(c(t(mat.y)))

  #Expand mat.g
  mat.g.ex <-
    kronecker(t(mat.g), id.n)


  return(
    list(
      mat.y.ex = mat.y.ex,
      mat.g.ex = mat.g.ex,
      p = p,
      mat.g = mat.g,
      mat.s = mat.s,
      my.dates = my.dates,
      mat.y = mat.y,
      n.eq = n.eq
    )
  )
}
