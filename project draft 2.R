kwh_data_10min = read.csv("/Users/elijahamirianfar/My Drive/04. CSU FULLERTON 2023-2025/3. Summer 2024/MATH 531T/FINAL PROJECT/TALK TO VALERIE ABOUT THIS/kWh by 10 min.csv")
ts_10min = ts(kwh_data_10min)
plot(ts_10min[,2])
acf(ts_10min)
