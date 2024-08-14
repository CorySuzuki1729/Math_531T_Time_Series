library(readr)
library(dplyr)
library(lubridate)
library(rugarch)
library(rmgarch)
library(forecast)
library(tseries)

#import data
energydata_complete <- read_csv("C://Users/coryg/Downloads/energydata_complete.csv")

#data keep only the hour
hourly_data <- energydata_complete %>% 
  filter(minute(date) == 0)

#filter out all columns not needed and rename columns
data_subset <- subset(hourly_data, select = -c(lights, RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, T3, T4,T5,T6,T7,T8, Press_mm_hg, RH_out, Windspeed, Visibility, Tdewpoint, rv1,rv2))
colnames(data_subset) <-c("date", "Appliances", "Temp in Kitchen", "Temp in Living Room", "Temp in Parent Room", "Temp Outside")

#create relative change for appliance
rel_change = (data_subset$Appliances - lag(data_subset$Appliances)) /lag(data_subset$Appliances)
rel_change = rel_change[-1]
rel_change <- c(0,rel_change)


#combine the relative change into our dataset
data_subset <- cbind(data_subset, rel_change)

#test for stationarity
kpss.test(data_subset$rel_change)

#plot the relative change in the dataset
plot.ts(data_subset$rel_change, main = "Relative Change of Appliance (Wh)")

#using the garch package , see what the standard garch model uses
test_garch <- ugarchspec()

#use diff to change diff
rel_change_diff <- diff(data_subset$rel_change, differences = 1)

#picking variables for the arima model for the garch
#also use plots to determine stationarity of relative change
kpss.test(rel_change_diff)

#change the data into train and test data
train_rel_change_diff <- rel_change_diff[1:2631]
test_rel_change_diff <- rel_change_diff[2632:3289]

#try and find the parameters for the arma model
acf(train_rel_change_diff)
pacf(train_rel_change_diff)

#try auto arima to get the variables
parameters <- auto.arima(y= train_rel_change_diff, trace = TRUE)
parameters

#test 1 are parameters from the auto.arima
#test 2, 3 are parameters around test 1
#test 3 are the way off one to see if there are differences
test_garch1 <- ugarchspec(mean.model = list(armaOrder = c(1, 0)), variance.model = list(model = "sGARCH"))
test_garch2 <- ugarchspec(mean.model = list(armaOrder = c(1, 2)), variance.model = list(model = "sGARCH"))
test_garch3 <- ugarchspec(mean.model = list(armaOrder = c(1, 3)), variance.model = list(model = "sGARCH"))
test_garch4 <- ugarchspec(mean.model = list(armaOrder = c(10, 2)), variance.model = list(model = "sGARCH"))


# Print the GARCH specification
print(test_garch1)
print(test_garch2)
print(test_garch3)
print(test_garch4)

#testing our garch model
#omega is the intercept of the conditional variance model
#check that each weighted ljung-box test on standardized squared residuals are good
#standardized square residuals are the autocorrelation on the variance of the residuals
#IMPORTANT: Standard square residuals show no correlation because it would violate models assumption for constant vairance

#model 1
garch_model1 <- ugarchfit(spec = test_garch1, data = train_rel_change_diff)
garch_model1

#plot residuals diagonostics for the model 1
#par(mfrow = c(2, 2))
plot(garch_model1, which = 1) #conditional SD represents the volatility as predicted by the GARCH model vs our data
plot(garch_model1, which = 3) #When both series have peaks at the same time, it indicates that the GARCH model is capturing periods of high volatility accurately
plot(garch_model1, which = 9) #residuals are normally distributed
plot(garch_model1, which = 10) #the model residuals are uncorrelated

#model2 
garch_model2 <- ugarchfit(spec = test_garch2, data = train_rel_change_diff)
garch_model2

#plot residuals diagonostics for the model 2
#par(mfrow = c(2, 2))
plot(garch_model2, which = 1) #conditional SD represents the volatility as predicted by the GARCH model vs our data
plot(garch_model2, which = 3) #When both series have peaks at the same time, it indicates that the GARCH model is capturing periods of high volatility accurately
plot(garch_model2, which = 9) #residuals are normally distributed
plot(garch_model2, which = 10) #the model residuals are uncorrelated

#model 3
garch_model3 <- ugarchfit(spec = test_garch3, data = train_rel_change_diff)
garch_model3

#plot residuals diagonostics for the model 3
#par(mfrow = c(2, 2))
plot(garch_model3, which = 1) #conditional SD represents the volatility as predicted by the GARCH model vs our data
plot(garch_model3, which = 3) #When both series have peaks at the same time, it indicates that the GARCH model is capturing periods of high volatility accurately
plot(garch_model3, which = 9) #residuals are normally distributed
plot(garch_model3, which = 10) #the model residuals are uncorrelated

#model 4
garch_model4 <- ugarchfit(spec = test_garch4, data = train_rel_change_diff)
garch_model4

#plot residuals diagonostics for the model 4
#par(mfrow = c(2, 2))
plot(garch_model4, which = 1) #conditional SD represents the volatility as predicted by the GARCH model vs our data
plot(garch_model4, which = 3) #When both series have peaks at the same time, it indicates that the GARCH model is capturing periods of high volatility accurately
plot(garch_model4, which = 9) #residuals are normally distributed
plot(garch_model4, which = 10) #the model residuals are uncorrelated

#getting the coefficents from our model
garch_model1@fit$coef
garch_model2@fit$coef
garch_model3@fit$coef
garch_model4@fit$coef

#forecast on test data, with the same length as our test data
garch_forecast1 <- ugarchforecast(garch_model1, n.ahead = length(test_rel_change_diff))
#extract the fitted sigma from the forecast
vol_garch_forecast1 <- garch_forecast1@forecast[["sigmaFor"]]

#forecast on test data, with the same length as our test data
garch_forecast2 <- ugarchforecast(garch_model2, n.ahead = length(test_rel_change_diff))
#extract the fitted sigma from the forecast
vol_garch_forecast2 <- garch_forecast2@forecast[["sigmaFor"]]

#forecast on test data, with the same length as our test data
garch_forecast3 <- ugarchforecast(garch_model3, n.ahead = length(test_rel_change_diff))
#extract the fitted sigma from the forecast
vol_garch_forecast3 <- garch_forecast3@forecast[["sigmaFor"]]

#forecast on test data, with the same length as our test data
garch_forecast4 <- ugarchforecast(garch_model4, n.ahead = length(test_rel_change_diff))
#extract the fitted sigma from the forecast
vol_garch_forecast4 <- garch_forecast4@forecast[["sigmaFor"]]

#plot the forecast of our volatility
#par(mfrow = c(2,2))
plot(vol_garch_forecast1, ylim = c(1,1.8))
plot(vol_garch_forecast2, ylim = c(1,1.8))
plot(vol_garch_forecast3, ylim = c(1,1.8))
plot(vol_garch_forecast4, ylim = c(1,1.8))

#the forecasted on the differenced relative change
diff_garch_series1 <- garch_forecast1@forecast$seriesFor
diff_garch_series2 <- garch_forecast2@forecast$seriesFor
diff_garch_series3 <- garch_forecast3@forecast$seriesFor
diff_garch_series4 <- garch_forecast4@forecast$seriesFor

#objective measure of fit: SSE
sse1 <- sum((test_rel_change_diff - diff_garch_series1)^2)
sse2 <- sum((test_rel_change_diff - diff_garch_series2)^2)
sse3 <- sum((test_rel_change_diff - diff_garch_series3)^2)
sse4 <- sum((test_rel_change_diff - diff_garch_series4)^2)

#Neural Network Autoregressive Forecasting (30 lags, can change the number of predicted lags)

neuralnet_energy = nnetar(diff_garch_series4, lambda = 0)
autoplot(forecast(neuralnet_energy, h=30))

summary(neuralnet_energy)

#Prediction intervals for neural network AR forecast.

fcast_nnar = forecast(neuralnet_energy, PI = T, h=30)
autoplot(fcast_nnar)

#Bootstrapping/Bagging Forecasts
#100 simulations of time series with 30 lag predictions.
nsim = 100L
sim = bld.mbb.bootstrap(diff_garch_series4, nsim)

h = 30L
future = matrix(0, nrow = nsim, ncol = h)
for(i in seq(nsim))
  future[i,] = simulate(ets(sim[[i]]), nsim=h)

start = tsp(diff_garch_series4)[2]+1/12
simfc = structure(list(
  mean = ts(colMeans(future), start = start, frequency = 1),
  lower = ts(apply(future, 2, quantile, prob = 0.025),
             start = start, frequency = 1),
  upper = ts(apply(future, 2, quantile, prob = 0.975),
             start = start, frequency = 1),
  level = 95),
  class= "forecast")

etsfc = forecast(ets(diff_garch_series4), h = h, level = 95)
autoplot(diff_garch_series4) +
  ggtitle("Bootstrap Forecasts with Prediction Intervals") +
  xlab("Hours") + ylab("Energy Usage in Kwh") + autolayer(simfc, series = "Simulated") +
  autolayer(etsfc, series = "ETS")
