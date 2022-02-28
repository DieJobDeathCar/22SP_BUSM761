setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/IA_4")
rm(list=ls())
#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("fpp2")
#install.packages("tsbox")
library(tidyverse)
library(zoo)
library(fpp2)
library(ggplot2)
library(tsbox)

#### The IA4 data ###########

MyData <- read_csv("IA4data.csv")

###### preparation ##############

#note we do not have data, just the quarter indicator. We will
#therefore pretend that the information starts in the first quarter of 2017


#Creating a time series object - you need to set the correct frequency
sales.ts <- ts(MyData["Sales"], start = c(2016, 1), frequency = 4)

#use autoplot for time series
autoplot(sales.ts,ylab="Sales")

#############################

#split up the data (using the approach in the book) 
#update the X to be the number of periods in the validation period
nValid <- 4
nTrain <- length(sales.ts) - nValid

train.ts <- window(sales.ts, start = c(2016, 1), end = c(2016, nTrain))
valid.ts <- window(sales.ts, start = c(2016, nTrain + 1), 
                   end = c(2016, nTrain + nValid))

######### Now you are ready to start building models ###########


#fit an emultiplicative HW model
ses.train <- ets(train.ts, model="MAM", alpha = 0.2, beta = 0.15, gamma = 0.05)
ses.train.pred <- forecast(ses.train, h = nValid, level = 0)
ses.valid.pred <- forecast(ses.train, h = nValid, level = 0, newdata = valid.ts)
#plot the results
plot(train.ts, ylab = "Sales", xlab = "Quarter", bty = "l", 
     xaxt = "n", xlim = c(2016,2022), main = "Multiplicative Holt Winters")
axis(1, at = seq(2016, 2022, 1), labels = format(seq(2016, 2022, 1)))
lines(valid.ts, col = "grey20", lty = 3) #prediction from validation data
lines(ses.train.pred$fitted, lwd = 2, col = "blue", lty = 2) #fitted trend from training data
lines(ses.train.pred$mean, lwd = 2, col = "red", lty = 2) #prediction from training data

#quick plot
autoplot(ses.train.pred)
#performance
accuracy(ses.train.pred,sales.ts)
accuracy(ses.valid.pred,sales.ts)


###########Regression##############
peak.Q <- function(x){
  return(ifelse(x%%3==0, 1, 0))
}
reg.data<-data.frame(Quarter=MyData$Quarter, 
                     Sales=MyData$Sales, 
                     Trend=seq(1,24,1),
                     PeakQuarter=peak.Q(MyData$Quarter))

#run a regression model & prediction
reg.model<-lm(Sales~.,data=reg.data)
reg.pred<-predict(reg.model)
ts.reg.pred <- ts(reg.pred, start = c(2016, 1), frequency = 4)

#Plot
plot(train.ts, ylab = "Sales", xlab = "Quarter", bty = "l", 
     xaxt = "n", xlim = c(2016,2022), main = "Regression")
axis(1, at = seq(2016, 2022, 1), labels = format(seq(2016, 2022, 1)))
lines(valid.ts, lwd = 2, col = "grey20", lty = 3) #prediction from validation data
lines(ts.reg.pred, lwd = 2, col = "blue", lty = 3)

#get performance measures
accuracy(ts.reg.pred,sales.ts)
