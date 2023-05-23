
#####
#Several tests for variations in the number of variables for the following parameters:
#variables: 2, 3, 4, 5, and 10.
#intensity: 1
#max lags: 2
#estimation type: OLS
#trim: 0.15
#####

data("example_data")
#
mat.results <- data.frame(matrix(data = NA, nrow = 6, ncol = 1))
#
rownames(mat.results) <- c("F-stat SUP", "Identified index", "Conf. interval 90%", "Conf. interval 95%", "Conf. interval 99%", "Mean shift")
colnames(mat.results) <-  c("2")
#
#For 2 time series
list.results <- Main(mat.y = Simul(p = 2, when.break = 0.5), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.4, pos.break = FALSE)

mat.results[1, 1] <- list.results$max.f
mat.results[2, 1] <- list.results$break.ind
mat.results[3, 1] <- list.results$conf.interval[1]
mat.results[4, 1] <- list.results$conf.interval[2]
mat.results[5, 1] <- list.results$conf.interval[3]
mat.results[6, 1] <- list.results$mean.shift
