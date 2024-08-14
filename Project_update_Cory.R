library(readr)
library(dplyr)
library(lubridate)
library(rugarch)
library(rmgarch)
library(forecast)
library(tseries)
library(astsa)

#import data
energydata_complete <- read_csv("C://Users/coryg/Downloads/energydata_complete.csv")

energydata_complete$DATE <- as.POSIXct(energydata_complete$DATE, format = "%m/%d/%y %H:%M")

#data keep only the hour
hourly_data <- energydata_complete %>% 
  filter(minute(date) == 0)

#filter out all columns not needed and rename columns
data_subset <- subset(hourly_data, select = -c(lights, RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, T3, T4,T5,T6,T7,T8, Press_mm_hg, RH_out, Windspeed, Visibility, Tdewpoint, rv1,rv2))
colnames(data_subset) <-c("date", "Appliances", "Temp in Kitchen", "Temp in Living Room", "Temp in Parent Room", "Temp Outside")

plot(data_subset$Appliances, type = "l")
line(data_subset$`Temp in Living Room`)
cor(data_subset$Appliances[-1], data_subset$`Temp in Living Room`[-3290])
cor(data_subset$Appliances, data_subset$`Temp Outside`)
cor(data_subset$Appliances, data_subset$`Temp in Kitchen`)

plot( data_subset$`Temp Outside`,data_subset$Appliances)

#create the relative change
rel_change <- c(0, diff(data_subset$Appliances)/ lag(data_subset$Appliances))
rel_change <- na.omit(rel_change)

#combine the relative change into our dataset
data_subset <- cbind(data_subset, rel_change)

#plot the relative change in the dataset
plot.ts(data_subset$rel_change)



#using the garch package , see what the standard garch model uses
test_garch <- ugarchspec()

#picking variables for the arima model for the garch
#also use plots to determine stationarity of relative change
acf(data_subset$rel_change)
Pacf(data_subset$rel_change)

#kpss test to make sure our relative change is stationary
kpss.test(data_subset$rel_change)

#try auto arima to get the variables
parameters <- auto.arima(y= data_subset$rel_change)

#trying an arima(2=4,0,1), parameters from the auto.arima
test_garch <- ugarchspec(mean.model = list(armaOrder = c(4, 1)), variance.model = list(model = "sGARCH"))

# Print the GARCH specification
print(test_garch)

#testing our garch model
garch_model <- ugarchfit(spec = test_garch, data = data_subset$rel_change)

#note from output: eighted ljung box test, shows there is no signficant autocorrelation in the resdiuals

#list of things from our model
names(garch_model@fit) 

#getting the coefficents from our model
garch_model@fit$coef

#get the estimated conditional variance
model_var <- garch_model@fit$var

#get the squred residuals from our model
model_resid2 <- (garch_model@fit$residuals)^2

#plot our squared residuals and the estimated conditional variance
plot(model_resid2, type = "l")
lines(model_var, col = "red")

#try to forcast the next 14 days
garch_forecast <- ugarchforecast(garch_model, n.ahead =336)
garch_forecast

#plot the variances from our forecast
garch_forecast_var <- garch_forecast@forecast$sigmaFor
plot(garch_forecast_var, type = "l")

#plot our last month with our forecasted values 
#plots look bad... need to work on it
var_data_last_month <- c(tail(model_var, 720), rep(NA, 10))
resid2_last_month <- c(tail(model_resid2, 720), rep(NA, 10))
garch_forecast_var_2_weeks <- c(rep(NA, 730), garch_forecast_var)

plot(resid2_last_month, type = "l", ylim = range(c(resid2_last_month, var_data_last_month, garch_forecast_var_2_weeks), na.rm = TRUE))
lines(var_data_last_month, col = "red")
lines(garch_forecast_var, col = "green")
legend("topright", legend = c("Squared Residuals", "Model Estimated Variance", "GARCH Forecasted Variance"), col = c("black", "red", "green"), lty = 1)

#Advanced Forecasting Techniques (Can choose one or any that you guys like)

#Neural Network Autoregressive Forecasting (30 lags, can change the number of predicted lags)

neuralnet_energy = nnetar(data_subset$rel_change, lambda = 0)
autoplot(forecast(neuralnet_energy, h=30))

#Prediction intervals for neural network AR forecast.

fcast_nnar = forecast(neuralnet_energy$rel_change, PI = T, h=30)
autoplot(fcast_nnar)

#Bootstrapping/Bagging Forecasts
#100 simulations of time series with 30 lag predictions.
nsim = 100L
sim = bld.mbb.bootstrap(data_subset$rel_change, nsim)

h = 30L
future = matrix(0, nrow = nsim, ncol = h)
for(i in seq(nsim))
  future[i,] = simulate(ets(sim[[i]]), nsim=h)

start = tsp(data_subset$rel_change)[2]+1/12
simfc = structure(list(
  mean = ts(colMeans(future), start = start, frequency = 60),
  lower = ts(apply(future, 2, quantile, prob = 0.025),
             start = start, frequency = 60),
  upper = ts(apply(future, 2, quantile, prob = 0.975),
             start = start, frequency = 60),
  level = 95),
  class= "forecast")

etsfc = forecast(ets(data_subset$rel_change), h = h, level = 95)
autoplot(data_subset$rel_change) +
  ggtitle("Bootstrap Forecasts with Prediction Intervals") +
  xlab("Hours") + ylab("Energy Usage in Kwh") + autolayer(simfc, series = "Simulated") +
  autolayer(etsfc, series = "ETS")


