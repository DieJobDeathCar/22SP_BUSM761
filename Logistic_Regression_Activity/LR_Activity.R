rm(list=ls())
#install.package("readr")
#install.package("tidyr")
#install.package("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("pacman")
#load library
#library(readr)
#library(tidyr)
#library(dplyr)
setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/Logistic_Regression_Activity")
library(tidyverse)
library(forecast)
library(pacman)
p_load(tidyverse,caret,ROCR)
Credit <- read_csv("credit_dataonly.csv")
```



#Let's take care of the necessary data manipulations
```{r}
#ensure R reads the categorical variables as categorical
Credit$GENDER<-factor(Credit$GENDER)
Credit$CHK_ACCT<-factor(Credit$CHK_ACCT)
Credit$SAV_ACCT<-factor(Credit$SAV_ACCT)
Credit$HISTORY<-factor(Credit$HISTORY)
Credit$PRESENT_RESIDENT<-factor(Credit$PRESENT_RESIDENT)
Credit$EMPLOYMENT<-factor(Credit$EMPLOYMENT)
Credit$JOB<-factor(Credit$JOB)
Credit$TYPE<-factor(Credit$TYPE)

#create the dependent variable
Credit <- Credit %>% mutate (IsProfitable=ifelse(NPV>0,1,0))

```

Per the instructions:
```{r}
set.seed(1)
```

Split up the Credit data into 70% training and 30% testing : 
```{r}
Train <- Credit %>% sample_frac(0.7) 
Validation <- Credit %>% anti_join(Train, by="OBS") 
```


#Our goal is to classify the customers as profitable or not using logistic regression:
#UPDATED: included changes from in class activity 020322
```{r}
lr1 <- glm(IsProfitable ~ .-OBS, data=Train, family="binomial")
#summary(lr1)
step.lr1 <- step(lr1,direction="forward",trace=0)
#summary(step.lr1)
```


To create a confusion matrix we first need to get the predictions, then we can create a confusion matrix:

```{r}
#first we predict the probability of preferring light beer
predTrain <- predict(lr1, type="response")
predVal <- predict(lr1, type="response",newdata = Validation)

#then we provide the confusionmatrix function with the classification based 
#on our selected cut-off and the actual outcomes
confusionMatrix(as.factor(ifelse(predTrain > 0.5, 1, 0)), as.factor(Train$IsProfitable), positive="1")

confusionMatrix(as.factor(ifelse(predVal > 0.23, 1, 0)), as.factor(Validation$IsProfitable), positive="1")

#Define the misclassification costs based on console output from confusion matrix
#1 and 2 are just example numbers
cost.fp = 2 
cost.fn = 1

# create empty error measure tables
MyCost = c()

# compute cost per cutoff
for (cut in seq(0,1,0.01)){
  #finding the confusion matrix for each cutoff
  cm <- confusionMatrix(as.factor(1 * (predprob > cut)), as.factor(Validation$IsProfitable), positive = "1")
  #calculate the misclassification cost
  FP = cm$table[2,1]
  FN = cm$table[1,2]
  MyCost = c(MyCost, FP*cost.fp+FN*cost.fn)
}

#plot the misclassification costs
#plot(seq(0,1,0.01), MyCost, xlab = "CutOff", ylab = "Misclassification Costs",type = "l")

#If you want to create the analysis in Excel, here is the code to export the results:
#add the predictions to the training and testing sets 
#Train<-data.frame(Train, "Predicted" = predTrain)
#Validation<-data.frame(Validation, "Predicted" = predVal)

#add an indicator of whether the data was in the Training or Validation
#Train <- Train %>% mutate(MySample="T")
#Validation <- Validation %>% mutate(MySample="V") 
  
#combine the training and testing
#Credit<-rbind(Train,Validation)

#write the results to a flat file 
write.csv(Credit,"LRactivityOutput.csv", row.names = FALSE)
```
