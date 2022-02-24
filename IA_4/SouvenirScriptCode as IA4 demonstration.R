setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/IA_4")
rm(list=ls())
#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("fpp2")

library(tidyverse)
library(zoo)
library(fpp2)

#### The souvenir data ###########

MyData <- read_csv("SouvenirSales.csv")

#translate Date into Date
MyData <-MyData %>% mutate(Date=as.Date(Date,"%m/%d/%y"))


###### preparation ##############

#Creating a time series object
Sales.ts <- ts(MyData["Sales"], start = c(2015, 1), frequency = 12)

#use autoplot for time series
autoplot(Sales.ts,ylab="Sales")

#cool stuff
autoplot(decompose(Sales.ts))

#############################

#split up the data (using the approach in the book)
nValid <- 12
nTrain <- length(Sales.ts) - nValid

train.ts <- window(Sales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(Sales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))

################ naive prediction #############

naive_fc <- naive(train.ts, h = 12)

#summarize 
summary(naive_fc)

# Use accuracy() to compute forecast accuracy
# note: use the full series
accuracy(naive_fc, Sales.ts)


# plot the series
plot(train.ts, ylab = "Sales", xlab = "Time", bty = "l", ylim=c(0,110000),
     xaxt = "n", xlim = c(2015,2022), main = "Naive Forecast")
axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))
lines(valid.ts, col = "grey20", lty = 3) #add the validation data
lines(naive_fc$mean, lwd = 2, col = "blue", lty = 2) #add the prediction for the validation
lines(naive_fc$fitted, lwd = 2, col = "blue", lty = 1) #add the prediciton for the training

#via autoplot
autoplot(naive_fc)



#########Additive Holt Winters


#note, you can also set the alpha, beta and gamma parameters
#if you do not set alpha, beta, gamma, then the ets() will try to optimize them 
#based on the TRAINING data, which may not be optimal for the VALIDATION data
hw.model <- ets(train.ts, model="AAA") 
hw.pred <- forecast(hw.model, h = nValid, level = 0)

#get a summary
summary(hw.model)

#performance
hwacc<-accuracy(hw.pred,Sales.ts)
write.csv(hwacc,"hwacc.csv") #output the performance

#plot to screen
plot(train.ts, ylab = "Sales", xlab = "Time", bty = "l", ylim=c(0,110000),
     xaxt = "n", xlim = c(2015,2022), main = "Additive Holt Winters")
axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))
lines(valid.ts, col = "grey20", lty = 3)
lines(hw.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(hw.pred$fitted, lwd = 2, col = "blue", lty = 1)


#plot to file start
png(file="hw_AAA.png")
#plot the results
plot(train.ts, ylab = "Sales", xlab = "Time", bty = "l", ylim=c(0,110000),
     xaxt = "n", xlim = c(2015,2022), main = "Additive Holt Winters")
axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))
lines(valid.ts, col = "grey20", lty = 3)
lines(hw.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(hw.pred$fitted, lwd = 2, col = "blue", lty = 1)
dev.off()
#plot to file end

#quick plot
autoplot(hw.pred)

#########Multiplicative Holt Winters ############################

#see comment above about setting alpha, beta and gamma
hw.model <- ets(train.ts, model="MAM")
hw.pred <- forecast(hw.model, h = nValid, level = 0)

#get a summary
summary(hw.model)


#plot the results
plot(train.ts, ylab = "Sales", xlab = "Time", bty = "l", ylim=c(0,110000),
     xaxt = "n", xlim = c(2015,2022), main = "Multiplicative Holt Winters")
axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))
lines(valid.ts, col = "grey20", lty = 3)
lines(hw.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(hw.pred$fitted, lwd = 2, col = "blue", lty = 1)


#quick plot
autoplot(hw.pred)

#performance
hwacc<-accuracy(hw.pred,Sales.ts)
write.csv(hwacc,"hwaccMult.csv")
