#' @title Main
#' @description Entry point for the whole computation of the algorithm of Bai, Lumsdaine, and Stock (1998)
#' @param mat.y The matrix object of time series
#' @param mat.x The matrix of optional covariates
#' @param trend Whether we add a trend. Default = FALSE
#' @param intercept Whether the break test is on the intercept only. Default = TRUE
#' @param ci A vector of confidence intervals. Default = c(0.9, 0.95, 0.99)
#' @param est.mode Estimation mode. Can be "OLS", "FGLS", or "IGLS"
#' @param iter Maximum number of iterations in the "IGLS" mode. Default to 3
#' @param aic.bic.mode Can be "AIC" or "BIC" depending on the criterion chosen for the lag selection
#' @param q.max Maximum lag tested for the AIC or BIC criterion
#' @param trim Percentage for the trim value for the starting and ending window over which the algorithm is not tested. Default to 15\%
#' @param pos.break Whether we want to select the maximum positive break only and discard the negative ones. Default to FALSE
#' @return A list of the vector of f-statistics, the maximum f-statistic retained, the confidence interval, the critical values, the break date, the original matrix of time series tested, the matrix with breaking and not breaking covariates, the index of the break in the time series, the size of the break (mean.shift), the optimal "AIC" or "BIC", a ggplot object (g1), and the trimmed dates.
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' \donttest{
#' data(example_data)
#' list.results <- Main(mat.y = example_data, q = 2)}

Main <- function(mat.y,
                 mat.x = NULL,
                 trend = FALSE,
                 intercept = TRUE,
                 ci = c(0.9, 0.95, 0.99),
                 est.mode = "OLS",
                 iter = 3,
                 aic.bic.mode = "AIC",
                 q.max = 2,
                 trim = 0.15,
                 pos.break = FALSE) {
  #get the variable names
  my.vars <- colnames(mat.y)

  #return the AIC and BIC criteria for lags from 1 to q.max
  q.opt <- AicBic(mat.y, q.max, mat.x, trend, intercept)

  #choose the lag q according to the min AIC
  q <- as.numeric(which.min(q.opt[aic.bic.mode, ]))
  message(paste0("Lag with the minimum ", aic.bic.mode, ": ", q))

  l.conf <-
    ConformableMatrix(
      mat.y = mat.y,
      q = q,
      mat.x = mat.x,
      trend = trend,
      intercept = intercept
    )

  #create a list of conform objects for the estimation
  #get the conformed (expanded) Yex matrix (for the system, in vector form)
  mat.y.ex <- l.conf$mat.y.ex

  #get the conformed mat.g.ex matrix of regressors for the system
  mat.g.ex <- l.conf$mat.g.ex
  #final number of observations

  p <- l.conf$p

  #original matrix of regressors
  mat.g <- l.conf$mat.g
  #selection matrix
  mat.s <- l.conf$mat.s
  #matching original dependent variables matrix
  mat.y <- l.conf$mat.y
  #original number of equations/dependent variables
  n.eq <- l.conf$n.eq
  #matching dates
  my.dates <- l.conf$my.dates

  message(paste0("Number of equations in the system: ", n.eq))
  #create a vector of f_statistics for each k tested
  f.stat <- rep(NA, p)
  #create a vector with the evaluated size of the intercept difference
  mean.shift <- rep(NA, p)

  #create a matrix of confidence intervals for each k tested
  mat.ci <- matrix(data = NA,
                   nrow = p,
                   ncol = length(ci))

  #compute critical values for the vector of confidence intervals proposed
  cv <- Vdistr(ci)

  #start index
  start.ind <- round(trim * p)

  #end index
  end.ind <- round(p - trim * p)

  #loop over the k with a trimming date/burn period
  for (k in start.ind:end.ind) {

    #force filling the mat.g.ex matrix with 0 before and original values after k
    mat.g.ex.b <- mat.g.ex %*% t(mat.s)

    #bind the regressor and breaking regressor matrices together
    mat.g.ex.b[1:((k - 1) * (n.eq)), ] <- 0

    mat.z <- t(cbind(mat.g.ex, mat.g.ex.b))

    l.beta.sigma <- Beta(
      mat.z = mat.z,
      mat.y.ex = mat.y.ex,
      n.eq = n.eq,
      p = p,
      est.mode = est.mode,
      iter = iter
    )

    #get the vector of betas
    mat.beta <- l.beta.sigma$mat.beta

    #get the covariance matrix of errors
    mat.sigma <- l.beta.sigma$mat.sigma

    #get the length of the vector of betas
    p.beta <- length(mat.beta)

    #create a selection matrix to get only the betas of interest (breakings)

    #1 - case where only shift in intercept
    if (intercept) {
      mat.r <- matrix(data = 0,
                      nrow = n.eq,
                      ncol = p.beta)
      mat.r[, (p.beta - n.eq + 1):p.beta] <- diag(n.eq)
    }

    #2 - case where all parameters break
    if (!intercept) {
      mat.r <- matrix(data = 0,
                      nrow = p.beta / 2 ,
                      ncol = p.beta)
      mat.r[, (p.beta / 2 + 1):p.beta] <- diag(p.beta / 2)
    }

    #compute the F-statistic for the current k
    f.stat[k] <- Fstat(
      mat.r = mat.r,
      mat.beta = mat.beta,
      mat.z = mat.z,
      p = p,
      mat.sigma = mat.sigma
    )

    #compute the confidence interval for the current k
    mat.ci[k,] <- ConfidenceInterval(
      mat.g = mat.g,
      mat.s = mat.s,
      mat.sigma = mat.sigma,
      mat.r = mat.r,
      mat.beta = mat.beta,
      cv = cv,
      p = p
    )

    #get the mean intercept shift
    mean.shift[k] <- mean(mat.r %*% mat.beta)
  }

  #if pos.break is TRUE, limit to positive break detection
  if (pos.break)
    f.stat[mean.shift < 0] <- 0

  # dev.new()
  plot(f.stat)

  g1 <- PlotStats(
    my.dates = my.dates,
    my.vars = my.vars,
    f.stat = f.stat,
    mat.ci = mat.ci,
    mat.y = mat.y
  )

  break.ind <- which.max(f.stat)
  break.date <- my.dates[break.ind]
  break.ci <- mat.ci[break.ind, ]
  rownames(mat.y) <- my.dates
  mat.g.t <- t(mat.g)
  rownames(mat.g.t) <- my.dates
  mean.shift <- mean.shift[break.ind]
  max.f <- max(f.stat, na.rm = T)
  trim.dates <-
    matrix(
      data = c(my.dates[start.ind], my.dates[end.ind]),
      nrow = 1,
      ncol = 2
    )
  colnames(trim.dates) <- c("begin trim date", "end trim date")

  return(
    list(
      f.stat = f.stat,
      max.f = max.f,
      conf.interval = break.ci,
      critical.values = cv,
      break.date = break.date,
      mat.y = data.frame(mat.y),
      mat.g = data.frame(mat.g.t),
      break.ind = break.ind,
      mean.shift = mean.shift,
      aic.bic = q.opt,
      g1 = g1,
      trim.dates = trim.dates
    )
  )
}
