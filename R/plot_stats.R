#' @title PlotStats
#'
#' @description Generate a ggplot2 object to depict the break and the time series tested
#'
#' @param my.dates A vector of dates
#' @param my.vars The variables tested
#' @param f.stat The f-statistics
#' @param mat.ci The matrix of confidence intervals
#' @param mat.y The original time series
#'
#' @return A ggplot2 object
#' @export
#' @importFrom dplyr "%>%"



PlotStats <-
  function(my.dates, my.vars, f.stat, mat.ci = mat.ci, mat.y) {
    my.dates <- as.Date(my.dates, format = "%d.%m.%Y")

    #get the index when the break occurs
    max.f <- which.max(f.stat)

    #get the confidence intervals around the break
    cis <-
      round(mat.ci[max.f,])

    #hard code for three different confidence intervals

    start.date90 <- my.dates[max.f - cis[1]]
    end.date90 <- my.dates[max.f + cis[1]]

    start.date95 <- my.dates[max.f - cis[2]]
    end.date95 <- my.dates[max.f + cis[2]]

    start.date99 <- my.dates[max.f - cis[3]]
    end.date99 <- my.dates[max.f + cis[3]]

    p <- dim(mat.y)[1]
    n <- dim(mat.y)[2]

    #get values in percentage
    mat.y <-
      mat.y * 100

    mat.y <- apply(mat.y, 2, as.double)
    mat.y <- data.frame(cbind(my.dates, mat.y))

    colnames(mat.y)[1] <- "Date"
    colnames(mat.y)[2:(n + 1)] <- my.vars

    mat.y[, 1] <- my.dates

    #reshape to long format
    mat.y <-
      melt(mat.y, id.var = "Date")
    names(mat.y)[2] = "Variables"

    g1 <-
      ggplot(mat.y,
             aes(
               x = Date,
               y = value,
               group = Variables,
               colour = Variables
             ))
    g1 <- g1  + geom_line()

    g1 <-
      g1 + ggtitle("") + xlab("Date") + ylab("Index investment (%)")

    #force the y axis to start at zero
    g1 <-
      g1 + scale_y_continuous(expand = c(0, 0))

    g1 <- g1 + scale_x_date(breaks = scales::pretty_breaks(n = 10))

    g1 <- g1 + theme_bw()

    #add shaded area for various ci
    g1 <-
      g1 + annotate(
        "rect",
        xmin = start.date90,
        xmax = end.date90,
        ymin = -Inf,
        ymax = Inf,
        alpha = .6
      )
    g1 <-
      g1 + annotate(
        "rect",
        xmin = start.date95,
        xmax = end.date95,
        ymin = -Inf,
        ymax = Inf,
        alpha = .4
      )
    g1 <-
      g1 + annotate(
        "rect",
        xmin = start.date99,
        xmax = end.date99,
        ymin = -Inf,
        ymax = Inf,
        alpha = .2
      )

    d <-
      data.frame(date = my.dates[max.f], event = "index investment break")
    print(d)
    #add the break line
    g1 <-
      g1 + geom_vline(
        data = d,
        mapping = aes(xintercept = date),
        color = "black",
        size = 1
      )
    dev.new()
    return(g1)
  }