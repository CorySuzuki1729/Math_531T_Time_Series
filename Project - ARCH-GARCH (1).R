library(readr)
library(dplyr)
library(lubridate)
library(rugarch)
library(rmgarch)

#import data
energydata_complete <- read_csv("/Users/elijahamirianfar/My Drive/04. CSU FULLERTON 2023-2025/3. Summer 2024/MATH 531T/FINAL PROJECT/energydata_complete.csv")

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

plot(data_subset$`Temp Outside`,data_subset$Appliances)

#create the relative change
rel_change = (data_subset$Appliances - lag(data_subset$Appliances)) /lag(data_subset$Appliances)
rel_change = c(0,rel_change1)

#create the relative change
#rel_change2 <- c(0,diff(data_subset$Appliances)/lag(data_subset$Appliances))
#rel_change2 <- na.omit(rel_change2)

'diff(data_subset$Appliances)[1:10]
data_subset$Appliances[1:10]
data_subset$Appliances[1:10]
rel_change1[1:10]
rel_change2[1:10]'

#combine the relative change into our dataset
data_subset <- cbind(data_subset, rel_change)

#plot the relative change in the dataset
plot.ts(data_subset$rel_change, main = "Relative Change Over Time",
        ylab = "Relative Change (kWh)")

#using the garch package, see what the standard garch model uses
test_garch <- ugarchspec()

#picking variables for the arima model for the garch
par(mfrow = c(1,2))
acf(data_subset$rel_change, main = "ACF Plot")
acf(data_subset$rel_change, type = "partial", main = "PACF Plot",ylim = c(-0.2,1))

#kpss test to make sure our relative change is stationary
kpss.test(data_subset$rel_change)
kpss.test(diff(data_subset$rel_change,1))

par(mfrow = c(1,2))
acf(diff(data_subset$rel_change,1), main = "ACF Plot")
acf(diff(data_subset$rel_change,1), type = "partial", main = "PACF Plot",ylim = c(-0.2,1))

3290*0.8

#checking for non constant variance
BoxCox.lambda(diff(data_subset$rel_change,1))

#trying an arima(6,0,1)
test_garch <- ugarchspec(mean.model = list(armaOrder = c(6, 1)), 
                         variance.model = list(model = "sGARCH",
                                               variance.targeting = T))

# Print the GARCH specification
print(test_garch)

#trying an arima(2,0,1)
test_garch2 <- ugarchspec(mean.model = list(armaOrder = c(2, 1)), 
                          variance.model = list(model = "sGARCH",
                                                variance.targeting = T))

# Print the GARCH specification
print(test_garch2)

#testing our garch model
garch_model <- ugarchfit(spec = test_garch, data = data_subset$rel_change)
garch_model
garch_model2 <- ugarchfit(spec = test_garch2, data = data_subset$rel_change)
garch_model2
