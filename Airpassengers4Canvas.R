########################
## AirPassengers Data ##
########################

# Calling AirPassengers data, making modelling and validation sets
data(AirPassengers)
modelling.data <- window(AirPassengers,1949,1959+11/12)
validation.data <- window(AirPassengers,1960,1960+11/12)

# plotting the data
par(mfrow=c(2,1))
plot(modelling.data)
abline(v = c(1953,1957))
group <- c(rep(1,4*12),rep(2,4*12),rep(3,3*12))
bartlett.test(as.numeric(modelling.data) , group)

plot(log(modelling.data))

# transformation due to non-constant variance
log.modelling.data <- log(modelling.data)

par(mfrow=c(3,1))
plot(log.modelling.data)
acf(log.modelling.data)
pacf(log.modelling.data)


# seasonal differencing in lag 12
B12x <- diff(log.modelling.data,lag=12)
par(mfrow=c(3,1))
plot(B12x)
acf(B12x,lag.max=36)
pacf(B12x,lag.max=36)


# regular differencing in lag 1
BB12x <- diff(B12x)
par(mfrow=c(3,1))
plot(BB12x)
acf(BB12x,lag.max=36)
pacf(BB12x,lag.max=36)


library(astsa)

# fitting the proposed models

fit1 <- sarima(log.modelling.data, p=0,d=1,q=1,P=0,D=1,Q=1,S=12)

fit2 <- sarima(log.modelling.data, p=0,d=1,q=1,P=1,D=1,Q=0,S=12)

fit3 <- sarima(log.modelling.data, p=1,d=1,q=0,P=0,D=1,Q=1,S=12)

fit4 <- sarima(log.modelling.data, p=1,d=1,q=0,P=1,D=1,Q=1,S=12)


# Forecasting based on the final 3 models
par(mfrow=c(3,1))
fore1 <- sarima.for(log.modelling.data, n.ahead=12, p=0,d=1,q=1,P=0,D=1,Q=1,S=12)
fore2 <- sarima.for(log.modelling.data, n.ahead=12, p=0,d=1,q=1,P=1,D=1,Q=0,S=12)
fore3 <- sarima.for(log.modelling.data, n.ahead=12, p=1,d=1,q=0,P=0,D=1,Q=1,S=12)

# forecasting based on SARIMA(0,1,1)x(0,1,1)_12
lower <- fore1$pred-1.96*fore1$se
upper <- fore1$pred+1.96*fore1$se
fit <- fore1$pred
yband <- c(0,1000)
ts.plot(AirPassengers,xlim=c(1949,1961),ylim=yband,ylab='Air Passengers',main='SARIMA(0,1,1)x(0,1,1)_12')
lines(exp(fit),col='red',type='b',pch='*')  	
lines(exp(lower),col='blue',lty=2)
lines(exp(upper),col='blue',lty=2)


# forecasting based on SARIMA(0,1,1)x(1,1,0)_12
lower <- fore2$pred-1.96*fore2$se
upper <- fore2$pred+1.96*fore2$se
fit <- fore2$pred
yband <- c(0,1000)
ts.plot(AirPassengers,xlim=c(1949,1961),ylim=yband,ylab='Air Passengers',main='SARIMA(0,1,1)x(1,1,0)_12')
lines(exp(fit),col='red',type='b',pch='*')    
lines(exp(lower),col='blue',lty=2)
lines(exp(upper),col='blue',lty=2)


# forecasting based on SARIMA(1,1,0)x(0,1,1)_12
lower <- fore3$pred-1.96*fore3$se
upper <- fore3$pred+1.96*fore3$se
fit <- fore3$pred
yband <- c(0,1000)
ts.plot(AirPassengers,xlim=c(1949,1961),ylim=yband,ylab='Air Passengers',main='SARIMA(1,1,0)x(0,1,1)_12')
lines(exp(fit),col='red',type='b',pch='*')    
lines(exp(lower),col='blue',lty=2)
lines(exp(upper),col='blue',lty=2)


# Calculating SS(pr) for the final 3 models
sum((exp(fore1$pred)-validation.data)^2)
sum((exp(fore2$pred)-validation.data)^2)
sum((exp(fore3$pred)-validation.data)^2)


#Forecasting the future 2 years. Notice that we are using the whole dataset this time.
future.forecast <- sarima.for(log(AirPassengers), n.ahead=24, p=0,d=1,q=1,P=1,D=1,Q=0,S=12)

lower <- future.forecast$pred-1.96*future.forecast$se
upper <- future.forecast$pred+1.96*future.forecast$se
fit <- future.forecast$pred
yband <- c(0,1000)
ts.plot(AirPassengers,xlim=c(1949,1963),ylim=yband,ylab='Air Passengers',main='SARIMA(0,1,1)x(1,1,0)_12')
lines(exp(fit),col='red',type='b',pch='*')    
lines(exp(lower),col='blue',lty=2)
lines(exp(upper),col='blue',lty=2)

