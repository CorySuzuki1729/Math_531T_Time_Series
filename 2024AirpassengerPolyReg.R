library(dlm)
library(forecast)
library(astsa)
library(ggplot2)


### Quick Nile River Example
# A reasonable model can be a random walk plus noise, with unknown state system and observation variances
#f = 1, G = 1 x
#Random walk plus noise, with unknown system and observation variances
data("Nile")
plot(Nile)
acf(Nile)
pacf(Nile)

dlmModPoly(1)
buildFun <- function(x) {
   dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
}

fit <- dlmMLE(Nile, parm = c(0,0), build = buildFun)
 fit$convergence
 exp(fit$par)
 dlmNile <- buildFun(fit$par)
 V(dlmNile) 
 W(dlmNile)
 FF(dlmNile)
 GG(dlmNile)
 
 dlmNileFilter <- dlmFilter(Nile, dlmNile)
 plot(Nile, type = "l")
 points(time(Nile),dlmNileFilter$m[-1], col = 2) #filtered states
 points(time(Nile),FF(dlmNile)%*%dlmNileFilter$m[-1], col =4 )

y.fit = FF(dlmNile)%*%dlmNileFilter$m[-1] # y -fit= F'X_t = X_t, since F =1 here 
 
# dlmNileFilter$m #state space value
  sse = sum(c(y.fit)  - Nile)^2 #  - Nile)^2 #fitted sse's
  sse

  
  
 #############################
 #Air passengers compare dlm to lm we did in inital analysis
#############################  
 plot(AirPassengers) # plotting the time-series data. Notice that the data is already in the form of a time series. To check this type AirPassengers and hit enter.
 
 plot(log(AirPassengers))  # Plotting the log-transform of data
 modelling.data <- window(log(AirPassengers),1949,1959+11/12)
 validation.data <- window(log(AirPassengers),1960,1960+11/12)
 
 tim.m <- time(modelling.data) #creates vector of time that TS was collected
 # match with dlm t'values to polynomial use:
 n <- length(tim.m)
 tim.m <- seq(1:n)
 cycle(modelling.data) #gives the positions in the cycle of each observation
 month.m <- as.factor(cycle(modelling.data))
 
 tim.v <- time(validation.data) #creates vector of time that TS was collected
 #matching dlm:
 n.v<-length(tim.v)
 tim.v <- seq(from=(n+1), to =(n+n.v), by=1)
 month.v <- as.factor(cycle(validation.data)) 
 
 tim2.m = tim.m^2
 mod <- as.data.frame(cbind(modelling.data, tim.m, tim2.m, month.m))
 colnames(mod)<-  c('air', 't', "t2", "month")

 reg <- lm(air ~ t + t2 + as.factor(month) , data = mod) 
 plot(reg$fitted, type="l") # plot of fitted values vs residuals 
 qqnorm(reg$residuals) #qq-plot of residuals
 qqline(reg$residuals) # plotting the line, along which the dots in qq-plot should lie 
 plot(reg$residuals) # plotting the residuals vs time
 abline(h=0,lty=2) # plotting a horizontal line at 0
 acf(reg$residuals) #sample acf plot of residuals
 reg.MLE.sig2 <- sum(reg$residuals^2)/length(modelling.data)
 summary(reg)
 reg$model
 tim2.v = tim.v^2
 new <- as.data.frame(cbind(validation.data, tim.v, tim2.v, month.v))
 colnames(new)<-  c('air', 't', "t2", "month")
 pred.lm <- predict(reg, new) #save predictions to compare with dlm
 

 #Building the DLM for the model above: Polyregression of order 2 and factors for seasonality
 #### To get the State Space structure for dlmMod function funtion print the function in R: 
 dlmModPoly(3)# SSM for 2nd order polynomial
 dlmModSeas(12) # SSM for 12 Factor Seasons
 # for 2 order poly in dlm for states at t>0:
 #At state t: Xt[1] = d0 + td1 + 2d2*(t-1)*t = d0 + tbd1 - 2*td2 + 2*d2t^2 = d0 + t(d1-2d2) +t^2(2*d2)
 #Xt[2] = d1 + d2 + d2t
 #Xt[3] = 2*d2
 #(d0, d1 + d2 ,2*d2) at t= 0is initial state of poly coefs
 
 
 m01 =c(mean(modelling.data), 0, 0)  
 
 model.build <- function(p) {
   return(
     dlmModPoly(3,dV = exp(p[1]),dW =c(rep(0,3)), m0 = m01) +  #in lm we only have one variance parameter,  
       dlmModSeas(12,dV = exp(p[2]), dW = c(rep(0, 12-1))) #thus the equivalent dlm would as well
   )
 }
 
 # Find MLEs of parameters
 var((modelling.data)) #use for inital parameters
 model.mle <- dlmMLE(modelling.data, parm=c(0,0), build=model.build)
 model.fit <- model.build(model.mle$par)
 exp(model.mle$par) 
 #dlm splits the variance of poly and season 50/50
 #in dlm the parameter is added to the process 2 times so must multilply by 2 to get total variance estimate   
 #0.002348174
 
 
 #MLE for sigma^2 is the biased MSE:
 lm.sig2.mle = sum(reg$residuals^2)/length(reg$residuals)
 #0.002099125 
 #dlm only slighty high
 


 model.filtered <- dlmFilter(modelling.data, model.fit)
 #regression coefficients in DLM are in the state vectors 
 #in the state space structure in dlmModPoly since the final 
 #state vector contains all data in data, it should be somewhat
 #close to lm est, however, it is easier to extract at t=0, so
 #we smooth to update the states in the past and X_0 is the vector 
 #containing the linear coefficients
 
model.smoothed <- dlmSmooth(modelling.data, model.fit)
model.smoothed$s[1,1:3] # go back to smoothed initial state t= 0 to get coefficient values
dlm.coefs <- c(model.smoothed$s[1,1:1],model.smoothed$s[1,2] - (model.smoothed$s[1,3]/2),model.smoothed$s[1,3]/2)  
dlm.coefs       

reg$coefficients[1:3]
# the difference in intercept estimates is the effect of month1!
# see https://www.jarad.me/courses/stat615/slides/DLMs/DLMs.pdf slides 44-46
 


 model.forecast <- dlmForecast(model.filtered, nAhead=12)
 a <- drop(model.forecast$a%*%t(FF(model.fit))) #on step predictions (fitted) from state values
 
 
 x <- index(log(AirPassengers))
 df.dlm <- rbind(
   data.frame(x=x[1:132], y=as.numeric(AirPassengers)[1:132], series="data"),
   data.frame(x=x[1:132], y=exp(apply(model.filtered$m[-1,1:2], 1, sum)), series="Filtered States"),
   data.frame(x=x[1:132], y=exp(apply(model.smoothed$s[-1,1:2], 1, sum)), series="Smoothed States")
 )
 g.dlm <- ggplot(subset(df.dlm, x>1950), aes(x=x, y=y, colour=series)) + geom_line()
 g.dlm
 
 df.comp <- rbind(
  data.frame(x=x, y=as.numeric(AirPassengers), series="data"),
 data.frame(x=x[133:144], y=exp(a), series="DLM forecast"),
 data.frame(x=x[133:144], y=exp(pred.lm ), series="LM forecast")
 
  )
 g.comp <- ggplot(subset(df.comp, x>1950), aes(x=x, y=y, colour=series)) + geom_line(aes(linetype =series))
 g.comp

#right on top!!
#Forecasting SSE
sse.dlm = sum(exp(a) - exp(validation.data))^2 
sse.lm = sum(exp(pred.lm ) - exp(validation.data))^2
sse.dlm
sse.lm
#eurika!!



#fitted residuals
fit.y = FF(model.fit)%*%t(model.smoothed$s) #post smoothing

res.dlm = fit.y[-1] - modelling.data #note the 1st state in Filter and Smooth is the initial state X_0
res.lm = reg$fitted.values - modelling.data
res.dlm
res.lm 
acf(res.dlm) 
pacf(res.dlm)
acf(res.lm)
pacf(res.lm)
#we are see the seasonal P = 1 in residuals

#models that can be written in linear state space form can be
#modelled in the dlm package...the benefit of doing this is you can
#modify more easily, for example, in dlm, we could include a state level variance
#try setting parameters for the dW = p[3:5] in the dlmModPoly, then compare model to lm


#try applying the SARIMA(0,1,1)(1,1,0)[12] model in dlm (see https://openforecast.org/adam/StateSpaceARIMA.html)
#take a look at https://cran.r-project.org/web/packages/smooth/vignettes/ssarima.html
#for fitting msarima!

############ ############ ############ ############ 
############ missing data example
############ ############ ############ ############ 


comp.x <- log(AirPassengers)
incomp.x <- comp.x
# set some missing values
incomp.x[c(10,60:71,100,130)] <- NA
plot(incomp.x)
# fit best sarima model from class. use arima to prevent errors in plot for incomplete data 
# Incomplete with NA values 
it.inc <- arima(incomp.x, order=c(0,1,1),seasonal=list(order=c(1,1,0),period = 12))
it.inc$coef #arima() seems to handle NA's reasonable well
it.inc$residuals #note the residuals ar NA for missing 
#see Fitting methods in stats::arima help - Kalman Filtering is applied behind the scenes (so arima actually fits model in state space form!)
#thus the missing y's are imputed, but as mentioned in class, are not used to update posterior of state vectors via the error correction step

#The wrong way to model with NAs in Time Series
#Incomplete with NA's removed
rm.x <- incomp.x[-c(10,60:71,100,130)]
plot(rm.x,type="l") #notice the time marker has been deleted
fit.rm <- arima(rm.x, order=c(0,1,1),seasonal=list(order=c(1,1,0),period = 12)) #notice the bad residual effect
fit.rm$coef
#point here being the if you have NA's in time series, you CANNOT omit them 


#Complete data model
fit.com <- arima(comp.x, order=c(0,1,1),seasonal=list(order=c(1,1,0),period = 12))
fit.com$coef
#Compare the estimates of NA model (removed and not removed) and complete
summary(fit.rm)$coef #NAs removed
summary(it.inc)$coef #NAs kept
summary(fit.com)$coef #complete

#We cannot see the imputed y's from an arima model, but let's
#go back and fit the dlm poly seas model to the incomplete data:

model.mle.inc <- dlmMLE(incomp.x, parm=c(0,0), build=model.build)
model.fit.inc <- model.build(model.mle.inc$par)
model.filtered.inc <- dlmFilter(incomp.x, model.fit.inc)
K.1step.pred <- exp(model.filtered.inc$f )# the one step forecasts from Filtering
K.1step.pred[c(10,60:71,100,130)] #the time points of missing values
AirPassengers[c(10,60:71,100,130)]
filter.error <- sum((K.1step.pred[c(10,60:71,100,130)]-AirPassengers[c(10,60:71,100,130)])^2)
filter.error

#We can do better if we impute after smoothing!
model.smooth.inc <- dlmSmooth(incomp.x, model.fit.inc)
smooth.pred <- exp(FF(model.fit.inc)%*%t(model.smooth.inc$s[-1,])) 
smooth.pred[c(10,60:71,100,130)]
smooth.error <- sum((smooth.pred[c(10,60:71,100,130)]-AirPassengers[c(10,60:71,100,130)])^2)
smooth.error   
#BIG improvement!
x <- index(log(AirPassengers))
plot(AirPassengers)
lines(x,smooth.pred , col =2)
points(x[c(10,60:71,100,130)],smooth.pred[c(10,60:71,100,130)], col =3)
