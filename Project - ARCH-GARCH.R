library(readr)
library(dplyr)
library(lubridate)
library(rugarch)
library(rmgarch)
library(forecast)
library(tseries)
library(astsa)

#import data
#Note: If you are editing this R source file, please make sure to specify the
#correct path to your copy of the data file so the code will compile correctly.

energydata_complete <- read_csv("C://Users/coryg/Downloads/energydata_complete.xlsx - energydata_complete.csv")

#data keep only the hour
hourly_data <- energydata_complete %>% filter(minute(date) == 0)

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
acf(data_subset$rel_change)
acf(data_subset$rel_change,type = "partial")

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
