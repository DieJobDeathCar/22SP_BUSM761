#install.package("readr")
#install.package("tidyr")
#install.package("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("forecast")
#load library
#library(readr)
#library(tidyr)
#library(dplyr)
setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/Wk1_Inclass")
library(tidyverse)
library(forecast)
Credit<-read.csv("credit_dataonly.csv")
#basic
ggplot(Credit, aes(x=AGE, y=NPV)) + geom_point()
#to make our lives a little easier lets define which variables should be factors
Credit$GENDER<-factor(Credit$GENDER)
Credit$CHK_ACCT<-factor(Credit$CHK_ACCT)
Credit$SAV_ACCT<-factor(Credit$SAV_ACCT)
Credit$HISTORY<-factor(Credit$HISTORY)
Credit$PRESENT_RESIDENT<-factor(Credit$PRESENT_RESIDENT)
Credit$EMPLOYMENT<-factor(Credit$EMPLOYMENT)
Credit$JOB<-factor(Credit$JOB)
Credit$TYPE<-factor(Credit$TYPE)

#For all of us to get the same results, lets set the seed of the random number generator:
set.seed(1)

#Now lets split up the Credit data into 70% training and 30% testing : 
Train <- Credit %>% sample_frac(0.7) 
Validation <- Credit %>% anti_join(Train, by="OBS") 

#Our goal is to predict NPV based on the characteristics of the credit applicant. 
#use what you have learned from our exploration and add some key variables: 
lm1 <- lm(NPV ~ AGE + CHK_ACCT+ SAV_ACCT + NUM_CREDITS + DURATION, data=Train)
summary(lm1)

#Analyzing predictive performance:
Lm_Train_Predictions <- predict(lm1)
Lm_Validation_Predictions <- predict(lm1, newdata=Validation)

# training
accuracy(Lm_Train_Predictions, Train$NPV)
# validation
accuracy(Lm_Validation_Predictions, Validation$NPV)

#can you do better? 

#we can also implement variable selection 

############ adding varible selection #################

#resplit
Train <- Credit %>% sample_frac(0.7) 
Validation <- Credit %>% anti_join(Train, by="OBS") 

#Now we can run a stepwise regression, it takes two steps: 
#first we need to define the full model, then we feed that into the stepwise regression

#define a linear regression model using all variables except OBS and CREDIT_EXTENDED
lm1 <- lm(NPV ~ .-OBS -CREDIT_EXTENDED, data=Train)

#fit a backwards regression
lm.step <- step(lm1, direction = "backward")
summary(lm.step)  # Which variables did it drop?

#Your turn: get the predictions and performance measures

#to compare to forward selection or stepwise selction change the direciton to "forward" or "both"



#####################################################
############# PART III #############################
##################################################

#If you want to analyze the predictions in Excel, this code will add a sample
#indicator and the predictions to the Credit dataset

#add the predictions to the training and testing sets 
Train<-data.frame(Train, "Predicted" = Lm_Train_Predictions)
Validation<-data.frame(Validation, "Predicted" = Lm_Validation_Predictions)

#add an indicator of whether the data was in the Training or Validation
Train <- Train %>% mutate(MySample="T")
Validation <- Validation %>% mutate(MySample="V") 

#combine the training and testing
Credit<-rbind(Train,Validation)

#write the results to a flat file 
write.csv(Credit,"credit_with_predictions.csv", row.names = FALSE)