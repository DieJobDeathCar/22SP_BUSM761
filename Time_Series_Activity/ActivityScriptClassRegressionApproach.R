rm(list=ls())

install.packages("tidyverse")
install.packages("fpp2")

library("tidyverse")
library("fpp2")

#### The bikeshare data ###########
MyData <- read_csv("DCbikeshare.csv")

#translate Date into Date
MyData <-MyData %>% mutate(Date=as.Date(Date,"%m/%d/%Y"))


##############################################################

#Now lets do regression

reg.data<-data.frame(Quantity=MyData$Quantity, 
                     Date=MyData$Date, 
                     Trend=seq(1,1842,1),
                     DayOfTheWeek=weekdays(MyData$Date),
                     Month=months(MyData$Date))

#what does the data look like?

#run a regression model
reg.model<-lm(Quantity~.-Date,data=reg.data)
summary(reg.model)

#get the predictions
reg.pred<-predict(reg.model)

#get performance measures
accuracy(reg.pred,MyData$Quantity)

#how would you plot the results?



################### modeling challenge ###############

#combine data
TheWeather <- read_csv("weather_test.csv")
reg.data<-cbind(reg.data,TheWeather)

#improve your model


