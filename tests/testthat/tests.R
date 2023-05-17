
#####
#Several tests for variations in the number of variables for the following parameters:
#variables: 2, 3, 4, 5, and 10.
#intensity: 1
#max lags: 2
#estimation type: OLS
#trim: 0.15
#####

data("example_data")

mat.results <- data.frame(matrix(data = NA, nrow = 6, ncol = 5))

rownames(mat.results) <- c("F-stat SUP", "Identified index", "Conf. interval 90%", "Conf. interval 95%", "Conf. interval 99%", "Mean shift")
colnames(mat.results) <-  c("2", "3", "4", "5", "10")

#For 2 time series
list.results <- Main(mat.y = Simul(p = 2), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 1] <- list.results$max.f
mat.results[2, 1] <- list.results$break.ind
mat.results[3, 1] <- list.results$conf.interval[1]
mat.results[4, 1] <- list.results$conf.interval[2]
mat.results[5, 1] <- list.results$conf.interval[3]
mat.results[6, 1] <- list.results$mean.shift

#For 3 time series
list.results <- Main(mat.y = Simul(p = 3), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 2] <- list.results$max.f
mat.results[2, 2] <- list.results$break.ind
mat.results[3, 2] <- list.results$conf.interval[1]
mat.results[4, 2] <- list.results$conf.interval[2]
mat.results[5, 2] <- list.results$conf.interval[3]
mat.results[6, 2] <- list.results$mean.shift

#For 4 time series
list.results <- Main(mat.y = Simul(p = 4), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 3] <- list.results$max.f
mat.results[2, 3] <- list.results$break.ind
mat.results[3, 3] <- list.results$conf.interval[1]
mat.results[4, 3] <- list.results$conf.interval[2]
mat.results[5, 3] <- list.results$conf.interval[3]
mat.results[6, 3] <- list.results$mean.shift

#For 5 time series
list.results <- Main(mat.y = Simul(p = 5), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 4] <- list.results$max.f
mat.results[2, 4] <- list.results$break.ind
mat.results[3, 4] <- list.results$conf.interval[1]
mat.results[4, 4] <- list.results$conf.interval[2]
mat.results[5, 4] <- list.results$conf.interval[3]
mat.results[6, 4] <- list.results$mean.shift

#For 10 time series
list.results <- Main(mat.y = Simul(p = 10), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 5] <- list.results$max.f
mat.results[2, 5] <- list.results$break.ind
mat.results[3, 5] <- list.results$conf.interval[1]
mat.results[4, 5] <- list.results$conf.interval[2]
mat.results[5, 5] <- list.results$conf.interval[3]
mat.results[6, 5] <- list.results$mean.shift


print(mat.results)
# stargazer(mat.results, summary = FALSE, digits = 2, digits.extra = 2)


#####
#Several tests for variations in the intensity parameter:
#variables: 5
#intensity: 0.2, 0.5, 1, 1.5, 2
#max lags: 2
#estimation type: OLS
#trim: 0.15
#####

mat.results <- data.frame(matrix(data = NA, nrow = 6, ncol = 5))

rownames(mat.results) <- c("F-stat SUP", "Identified index", "Conf. interval 90%", "Conf. interval 95%", "Conf. interval 99%", "Mean shift")
colnames(mat.results) <-  c("0.2", "0.5", "1", "1.5", "2")

#For intensity = 0.2
list.results <- Main(mat.y = Simul(intensity = 0.2), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 1] <- list.results$max.f
mat.results[2, 1] <- list.results$break.ind
mat.results[3, 1] <- list.results$conf.interval[1]
mat.results[4, 1] <- list.results$conf.interval[2]
mat.results[5, 1] <- list.results$conf.interval[3]
mat.results[6, 1] <- list.results$mean.shift

#For intensity = 0.5
list.results <- Main(mat.y = Simul(intensity = 0.5), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 2] <- list.results$max.f
mat.results[2, 2] <- list.results$break.ind
mat.results[3, 2] <- list.results$conf.interval[1]
mat.results[4, 2] <- list.results$conf.interval[2]
mat.results[5, 2] <- list.results$conf.interval[3]
mat.results[6, 2] <- list.results$mean.shift

#For intensity = 1
list.results <- Main(mat.y = Simul(intensity = 1), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 3] <- list.results$max.f
mat.results[2, 3] <- list.results$break.ind
mat.results[3, 3] <- list.results$conf.interval[1]
mat.results[4, 3] <- list.results$conf.interval[2]
mat.results[5, 3] <- list.results$conf.interval[3]
mat.results[6, 3] <- list.results$mean.shift

#For intensity = 1.5
list.results <- Main(mat.y = Simul(intensity = 1.5), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 4] <- list.results$max.f
mat.results[2, 4] <- list.results$break.ind
mat.results[3, 4] <- list.results$conf.interval[1]
mat.results[4, 4] <- list.results$conf.interval[2]
mat.results[5, 4] <- list.results$conf.interval[3]
mat.results[6, 4] <- list.results$mean.shift

#For intensity = 2
list.results <- Main(mat.y = Simul(intensity = 2), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 5] <- list.results$max.f
mat.results[2, 5] <- list.results$break.ind
mat.results[3, 5] <- list.results$conf.interval[1]
mat.results[4, 5] <- list.results$conf.interval[2]
mat.results[5, 5] <- list.results$conf.interval[3]
mat.results[6, 5] <- list.results$mean.shift


print(mat.results)
# stargazer(mat.results, summary = FALSE, digits = 2, digits.extra = 2)





#####
#Several tests for variations in the maximum lags:
#variables: 5
#intensity: 1
#max lags: 1, 2, 3, 4, 5
#estimation type: OLS
#trim: 0.15
#####

mat.results <- data.frame(matrix(data = NA, nrow = 6, ncol = 5))

rownames(mat.results) <- c("F-stat SUP", "Identified index", "Conf. interval 90%", "Conf. interval 95%", "Conf. interval 99%", "Mean shift")
colnames(mat.results) <-  c("1", "2", "3", "4", "5")

#max lags = 1
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 1, trim = 0.15, pos.break = FALSE)

mat.results[1, 1] <- list.results$max.f
mat.results[2, 1] <- list.results$break.ind
mat.results[3, 1] <- list.results$conf.interval[1]
mat.results[4, 1] <- list.results$conf.interval[2]
mat.results[5, 1] <- list.results$conf.interval[3]
mat.results[6, 1] <- list.results$mean.shift

#max lag = 2
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 2] <- list.results$max.f
mat.results[2, 2] <- list.results$break.ind
mat.results[3, 2] <- list.results$conf.interval[1]
mat.results[4, 2] <- list.results$conf.interval[2]
mat.results[5, 2] <- list.results$conf.interval[3]
mat.results[6, 2] <- list.results$mean.shift

#max lag = 3
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 3, trim = 0.15, pos.break = FALSE)

mat.results[1, 3] <- list.results$max.f
mat.results[2, 3] <- list.results$break.ind
mat.results[3, 3] <- list.results$conf.interval[1]
mat.results[4, 3] <- list.results$conf.interval[2]
mat.results[5, 3] <- list.results$conf.interval[3]
mat.results[6, 3] <- list.results$mean.shift

#max lag = 4
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 4, trim = 0.15, pos.break = FALSE)

mat.results[1, 4] <- list.results$max.f
mat.results[2, 4] <- list.results$break.ind
mat.results[3, 4] <- list.results$conf.interval[1]
mat.results[4, 4] <- list.results$conf.interval[2]
mat.results[5, 4] <- list.results$conf.interval[3]
mat.results[6, 4] <- list.results$mean.shift

#max lag = 5
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 5, trim = 0.15, pos.break = FALSE)

mat.results[1, 5] <- list.results$max.f
mat.results[2, 5] <- list.results$break.ind
mat.results[3, 5] <- list.results$conf.interval[1]
mat.results[4, 5] <- list.results$conf.interval[2]
mat.results[5, 5] <- list.results$conf.interval[3]
mat.results[6, 5] <- list.results$mean.shift


print(mat.results)
# stargazer(mat.results, summary = FALSE, digits = 2, digits.extra = 2)




#####
#Several tests for variations in the computation method
#variables: 5
#intensity: 1
#max lags: 2
#estimation type: OLS, FGLS, IGLS3, IGLS5, IGLS10
#trim: 0.15
#####
set.seed(123)
mat.results <- data.frame(matrix(data = NA, nrow = 6, ncol = 5))

rownames(mat.results) <- c("F-stat SUP", "Identified index", "Conf. interval 90%", "Conf. interval 95%", "Conf. interval 99%", "Mean shift")
colnames(mat.results) <-  c("OLS", "FGLS", "IGLS3", "IGLS5", "IGLS10")

#OLS
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "OLS", iter = 3, aic.bic.mode = "AIC", q.max = 1, trim = 0.15, pos.break = FALSE)

mat.results[1, 1] <- list.results$max.f
mat.results[2, 1] <- list.results$break.ind
mat.results[3, 1] <- list.results$conf.interval[1]
mat.results[4, 1] <- list.results$conf.interval[2]
mat.results[5, 1] <- list.results$conf.interval[3]
mat.results[6, 1] <- list.results$mean.shift

#FGLS
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "FGLS", iter = 3, aic.bic.mode = "AIC", q.max = 2, trim = 0.15, pos.break = FALSE)

mat.results[1, 2] <- list.results$max.f
mat.results[2, 2] <- list.results$break.ind
mat.results[3, 2] <- list.results$conf.interval[1]
mat.results[4, 2] <- list.results$conf.interval[2]
mat.results[5, 2] <- list.results$conf.interval[3]
mat.results[6, 2] <- list.results$mean.shift

#IGLS3 (iter = 3)
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "IGLS", iter = 3, aic.bic.mode = "AIC", q.max = 3, trim = 0.15, pos.break = FALSE)

mat.results[1, 3] <- list.results$max.f
mat.results[2, 3] <- list.results$break.ind
mat.results[3, 3] <- list.results$conf.interval[1]
mat.results[4, 3] <- list.results$conf.interval[2]
mat.results[5, 3] <- list.results$conf.interval[3]
mat.results[6, 3] <- list.results$mean.shift

#IGLS5 (iter = 5)
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "IGLS", iter = 5, aic.bic.mode = "AIC", q.max = 4, trim = 0.15, pos.break = FALSE)

mat.results[1, 4] <- list.results$max.f
mat.results[2, 4] <- list.results$break.ind
mat.results[3, 4] <- list.results$conf.interval[1]
mat.results[4, 4] <- list.results$conf.interval[2]
mat.results[5, 4] <- list.results$conf.interval[3]
mat.results[6, 4] <- list.results$mean.shift

#IGLS10 (iter = 10)
list.results <- Main(mat.y = Simul(), mat.x = NULL, trend = FALSE, intercept = TRUE, ci = c(0.9, 0.95, 0.99), est.mode = "IGLS", iter = 10, aic.bic.mode = "AIC", q.max = 5, trim = 0.15, pos.break = FALSE)

mat.results[1, 5] <- list.results$max.f
mat.results[2, 5] <- list.results$break.ind
mat.results[3, 5] <- list.results$conf.interval[1]
mat.results[4, 5] <- list.results$conf.interval[2]
mat.results[5, 5] <- list.results$conf.interval[3]
mat.results[6, 5] <- list.results$mean.shift


print(mat.results)
# stargazer(mat.results, summary = FALSE, digits = 2, digits.extra = 2)
