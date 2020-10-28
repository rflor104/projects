library(data.table)
library(FitARMA)
library(dplyr)
library(tidyr)
library(perARMA)
library(standardize)

hw3 <- as.data.table(fread("\\RICARDO/FORECAST/Homework_3/HW_3_Data.csv"))

head(hw3)

z3 <- hw3$Z3

z3_norm <- rnorm(z3,mean=1,sd=1)

y_z3 <- z3_norm/max(z3_norm)

z3_norm

acf(z3_norm,lag.max = 10,
    xlab = "lag #", ylab = 'ACF',main=' ')

### (b)

acf(z3, lag.max = NULL, type = c("correlation", "covariance","partial"), 
    plot = TRUE, na.action = na.contiguous, demean = TRUE,
    xlab = "time series", ylab = 'ACF',main='ACF Plot')

pacf(z3, lag.max = 10, plot = TRUE, na.action = na.fail,
     xlab = "time series", main='PACF Plot')

hw3

### (c)

r_arma <- FitARMA(z3, order = c(1,0,0), demean = TRUE)

acf(resid(r_arma)) 

resid_arma <- resid(r_arma)

summary(resid_arma)

Box.test(resid_arma,type="Ljung",lag=1,fitdf=1)

### (d)

r_arma_new <- FitARMA(z3, order = c(1,1,0), demean = TRUE)

acf(resid(r_arma_new))

pacf(resid(r_arma_new))

resid_arma_new <- resid(r_arma)

summary(r_arma)

summary(r_arma_new)

Box.test(resid_arma_new,type="Ljung",lag=1,fitdf=1)

### (e)

r_arma_2 <- FitARMA(z3, order = c(2,0,0), demean = TRUE)

acf(resid(r_arma_2))

pacf(resid(r_arma_2))

summary(r_arma_2)

resid_arma_2 = resid(r_arma_2)

Box.test(resid_arma_2, type="Ljung",lag=1,fitdf=1)
