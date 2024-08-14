#############
## PACF
#############

setwd("~/Dropbox (CSU Fullerton)/My Math 531T Time Series/9 - SARIMA") #Will change for YOU!!!!

plot(ARMAacf(ar = c(0.7), ma = numeric(0), lag.max = 20, pacf = F), main="ACF", ylab=expression(rho(h)),xlab='h')
plot(ARMAacf(ar = c(0.7), ma = numeric(0), lag.max = 20, pacf = T), main="PACF", ylab=expression(alpha(h)),xlab='h')



###############
#ARIMA models
###############

data.sim <- arima.sim(n = 500, list(order = c(2,1,0), ar = c(0.7, -0.3)) , sd = sqrt(1))
season <- 5*sin(pi*time(data.sim)/6)
data.sim <- data.sim+season
par(mfcol=c(1,1))
plot(data.sim, main="ARIMA(2,1,0) simulation")
#dev.print(file="slide5arimasim1.pdf", dev=pdf)

acf(diffdata.sim)
acf(data.sim, type="partial")
#dev.print(file="slide5arimasim2.pdf", dev=pdf)

##########
#SARIMA
###########


temp.data <- scan(file='wine.txt')
wine <-  ts(temp.data, start = 1980,  frequency = 12)

par(mfrow=c(1,2))
plot(wine)
log.wine <- log(wine)
plot(log.wine,main='log.wine=log(wine)')


par(mfrow=c(1,1))
acf(log.wine,main='ACF of log transformed data',lag.max=36)


B12.data <- diff(log.wine,lag=12)

par(mfrow=c(1,2))
plot(B12.data,main='differenced data in lag 12')
acf(B12.data,lag.max=36)



acf(B12.data,lag.max=36)
pacf(B12.data,lag.max=36)


a<-arima(log(wine),order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12)) 
resid <- a$residuals


par(mfrow=c(1,3)) 
plot(resid) 
abline(h=0,lty=2)
acf(a$residuals)
qqnorm(resid) 
qqline(resid)
     

fit <- auto.arima(log(wine), trace = TRUE)

library(forecast)

pred <- forecast(fit, h =12, level = 0.95)

pred
plot(pred)
