#' @title Simul
#'
#' @description #Simulate data to test the functions
#'
#' @param n The number of time series observations
#' @param p The number of time series
#' @param intensity The intensity of the break
#' @param when.break When should the break be simulated (as a percentage of the time series sample)
#'
#' @return A matrix of time series with a common break
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats rnorm
#' @importFrom utils head

Simul <- function(n = 100,
                  p = 5,
                  intensity = 1,
                  when.break = 0.35) {
  #optional
  set.seed(123)

  pre.break.n <- round(n * when.break)
  post.break.n <- round(n * (1 - when.break))

  mat.y <- rbind(
    matrix(rnorm(pre.break.n * p) + 0
           , nrow = pre.break.n, ncol = p)
    ,
    matrix(
      rnorm(post.break.n * p) + 0 + intensity
      ,
      nrow = post.break.n,
      ncol = p
    )
  )

  #date when I wrote this code
  start.date <-
    as.Date("10.05.2023", format = "%d.%m.%Y")

  simul.dates <- 0:(n - 1) + start.date
  simul.dates <- format(simul.dates, format = "%d.%m.%Y")

  rownames(mat.y) <- simul.dates
  colnames(mat.y) <- LETTERS[1:p]

  print(head(mat.y))
  return(mat.y)
}
