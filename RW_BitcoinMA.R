library(astsa) 
library(ggplot2)
library(dplyr)
library(tidyquant) #geom_ma
library(seismicRoll)

#White Noise
set.seed(2)
w <- rnorm(500)
plot(w,type='s')
abline(h=0,col='red',lty=2,lwd=2)




#Random Walk
rw <- c(0)
for(i in 2:500){
  rw[i] <- rw[i-1]+sample(c(-1,1),1)
}
plot(rw,type='s')
abline(h=0,col='red',lty=2,lwd=2)


#White Noise Moving average

w3 <- stats::filter(w, filter = rep(1/3, 3), sides = 2) 
plot(w,type='s')
abline(h=0,col='red',lty=2,lwd=2)
lines(w3, col = "blue")

#White Noise Autoregressive
war <- stats::filter(w, filter =c(1,-0.9), method = "recursive")
plot(w,type='s', ylim=c(-6,6))
abline(h=0,col='red',lty=2,lwd=2)
lines(war, col = "blue")



#Moving Average with BTC (bitcoin) stock data
#https://www.nasdaq.com/market-activity/cryptocurrency/btc/historical
BTC <- read.csv("/Users/valeriepoynor/Dropbox (CSU Fullerton)/My Math 531T Time Series/Summer 2023/1-RW_MA/HistoricalData_1685562671511.csv", header =T)
head(BTC)
View(BTC)

BTC$Date <-as.Date(BTC$Date, "%m/%d/%Y")


p <- ggplot(BTC, aes(x = Date,y=Close.Last)) +
  geom_line(alpha = 0.4) +              # semi-transparent line
  xlab("Date") +
  ylab("Daily Closing Value of Stock")

p 

p2 = p + geom_ma(ma_fun = SMA, n = 30,colour="red1")   
p2

p3 = p2 + geom_ma(ma_fun = SMA, n = 90,colour="royalblue")
p3

BTC %>% mutate(mean30 = seismicRoll::roll_mean(Close.Last, n = 30, align = "center"),
               mean90 = seismicRoll::roll_mean(Close.Last, n = 90, align = "center")) %>% 
ggplot() +
  geom_line(aes(x = Date,y=Close.Last), alpha = 0.4) +              # semi-transparent line
  xlab("Date") +
  ylab("Daily Closing Value of Stock")+
  geom_line(aes(x = Date, y=mean30),color="red1") +
  geom_line(aes(x = Date, y=mean90),color="royalblue") 
  


#using basic plot and filter function

#mean30 <- roll_mean(BTC$Close.Last, n = 30, align = "center")
#mean90 <- roll_mean(BTC$Close.Last, n = 90, align = "center")
mean30 <- stats::filter(BTC$Close.Last, filter = rep(1/30, 30), sides = 2) #use sides =2
mean90 <- stats::filter(BTC$Close.Last, filter = rep(1/90, 90), sides = 2) #use sides =2

plot(BTC$Date, BTC$Close.Last, type="l", xlab = "Date", ylab="Daily Closing Value of Stock")
lines(BTC$Date,mean30, col = "red")
lines(BTC$Date,mean90, col = "blue")




