setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/IA_1")
sessionInfo()
rm(list=ls())
library(tidyverse)
library(forecast)
Airlinedata<-read.csv("airlinedataupdate.csv")
colnames(Airlinedata)
head(Airlinedata)
#scatterplot using ggplot

ggplot(Airlinedata, aes(x=Airlinedata$DISTANCE, y=Airlinedata$FARE, color=as.factor(Airlinedata$SW))) + geom_point() +
  labs(
    title="Distance vs. Fare",
    subtitle = "Whether Southwest Airlines serves that route (1) or not (0)",
    x="Distance", y="Fare($)",color="Presence of SW")
SLOTdummy <- ifelse(Airlinedata$SLOT == "Free",1,0)
GATEdummy <- ifelse(Airlinedata$GATE == "Free",1,0)
SWVACATION <- as.numeric(Airlinedata$SW == 1 & Airlinedata$VACATION == 1)
#Free = 1, Controlled/Constrained = 0
Airlinedata <- Airlinedata %>%
  mutate(SLOTdummy, GATEdummy, SWVACATION)
Airlinedata <- Airlinedata %>%  
  select(-SLOT,-GATE,-S_CODE,-S_CITY,-E_CODE,-E_CITY)

#make life easier
#Factor = categorical data
#Airlinedata$ObsNum<-factor(Airlinedata$ObsNum)
#Airlinedata$S_CODE<-factor(Airlinedata$S_CODE)
#Airlinedata$S_CITY<-factor(Airlinedata$S_CITY)
#Airlinedata$E_CODE<-factor(Airlinedata$E_CODE)
#Airlinedata$E_CITY<-factor(Airlinedata$E_CITY)
#Airlinedata$SLOT<-factor(Airlinedata$SLOT)
#Airlinedata$GATE<-factor(Airlinedata$GATE)
#Airlinedata$COUPON<-factor(Airlinedata$COUPON)
Airlinedata$NEW<-factor(Airlinedata$NEW)
Airlinedata$VACATION<-factor(Airlinedata$VACATION)
Airlinedata$SW<-factor(Airlinedata$SW)
#Airlinedata$HI<-factor(Airlinedata$HI)
#Airlinedata$S_INCOME<-factor(Airlinedata$S_INCOME)
#Airlinedata$E_INCOME<-factor(Airlinedata$E_INCOME)
#Airlinedata$S_POP<-factor(Airlinedata$S_POP)
#Airlinedata$E_POP<-factor(Airlinedata$E_POP)
#Airlinedata$DISTANCE<-factor(Airlinedata$DISTANCE)
#Airlinedata$PAX<-factor(Airlinedata$PAX)
#Airlinedata$FARE<-factor(Airlinedata$FARE)
Airlinedata$SLOTdummy<-factor(Airlinedata$SLOTdummy)
Airlinedata$GATEdummy<-factor(Airlinedata$GATEdummy)
Airlinedata$SWVACATION<-factor(Airlinedata$SWVACATION)
head(Airlinedata)
set.seed(1)

#split up data into 70% training and 30% testing : 
Train <- Airlinedata %>% 
  sample_frac(0.7) 
Validation <- Airlinedata %>% 
  anti_join(Train, by="ObsNum") 



lm1 <- lm(FARE ~ COUPON + NEW + HI + S_INCOME + E_INCOME + S_POP + E_POP + SW + DISTANCE + PAX + VACATION + GATEdummy + SLOTdummy, data=Train)
#sink("Exhibit2.txt")
#summary(lm1)
#sink()
summary(lm1)
#Analyzing predictive performance:
Lm_Train_Predictions <- predict(lm1)
Lm_Validation_Predictions <- predict(lm1, newdata=Validation)

# training
accuracy(Lm_Train_Predictions, Train$FARE)
# validation
accuracy(Lm_Validation_Predictions, Validation$FARE)

#Section 3
lmCUSTOM <- lm(FARE ~ .-COUPON -SWVACATION, data=Train)
#fit a backwards regression
lm.step <- step(lmCUSTOM, direction = "backward")
summary(lm.step)
LmCUSTOM_Train_Predictions <- predict(lmCUSTOM)
LmCUSTOM_Validation_Predictions <- predict(lmCUSTOM, newdata=Validation)
#sink("lmCUSTOM_Output.txt")
#summary(lmCUSTOM)
#sink()
# training
accuracy(LmCUSTOM_Train_Predictions, Train$FARE)
# validation
accuracy(LmCUSTOM_Validation_Predictions, Validation$FARE)

#Section 4
lmSWVACATION <- lm(FARE ~ .-COUPON, data=Train)
#fit a backwards regression
lm.step <- step(lmSWVACATION, direction = "backward")
summary(lm.step)
lmSWVACATION_Train_Predictions <- predict(lmSWVACATION)
lmSWVACATION_Validation_Predictions <- predict(lmSWVACATION, newdata=Validation)
#sink("lmSWVACATION_output.txt")
#summary(lmSWVACATION)
#sink()
# training
accuracy(lmSWVACATION_Train_Predictions, Train$FARE)
# validation
accuracy(lmSWVACATION_Validation_Predictions, Validation$FARE)