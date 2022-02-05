rm(list=ls())
#install.package("readr")
#install.package("tidyr")
#install.package("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("pacman")
#install.packages("pkr")
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

#Let's take care of the necessary data manipulations

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

#Per the instructions:

set.seed(1)

#Split up the Credit data into 70% training and 30% testing : 

Train <- Credit %>% sample_frac(0.7) 
Validation <- Credit %>% anti_join(Train, by="OBS") 



#Our goal is to classify the customers as profitable or not using logistic regression:
#UPDATED: included changes from in class activity 020322
lr1 <- glm(IsProfitable ~ .-OBS - CREDIT_EXTENDED - NPV, data=Train, family="binomial")

#output a txt file containing the model
#sink("Exhibit1.txt")
#summary(lr1)
#sink()

#output step model into txt file
step.lr1 <- step(lr1,direction="backward",trace=0)
#sink("Exhibit1.txt")
#summary(step.lr1)
#sink()

````````````````````````````````````````````````````````````````````````````
#To create a confusion matrix we first need to get the predictions, then we can create a confusion matrix:
#first we predict the probability of preferring light beer
predTrain <- predict(lr1, type="response")
predVal <- predict(lr1, type="response",newdata = Validation)

#then we provide the confusionmatrix function with the classification based 
#Looping through the cutoff range and recording the cutoff point and accuracy from confusion matrix
cutoff_range = c(seq(0,1,0.01))
predTrain_accuracyLIST=c()
predTrain_cutoffLIST=c()
predVal_accuracyLIST=c()
predVal_cutoffLIST=c()


for (val in cutoff_range){
predTrain_cm <- confusionMatrix(as.factor(ifelse(predTrain >= val, 1, 0)), as.factor(Train$IsProfitable), positive="1")
predTrain_accuracyLIST=c(predTrain_accuracyLIST,predTrain_cm$overall[["Accuracy"]])
predTrain_cutoffLIST=c(predTrain_cutoffLIST,val)

}
#combining accuracy list with cutoff list
predTrain_TableOutput<-cbind(data.frame(predTrain_cutoffLIST),data.frame(predTrain_accuracyLIST))
#output into CSV table
write.csv(predTrain_TableOutput,"predTrain_CM_Table.csv", row.names = FALSE)

#looping predVal
for (val in cutoff_range){
predVal_cm <- confusionMatrix(as.factor(ifelse(predVal > val, 1, 0)), as.factor(Validation$IsProfitable), positive="1")
predVal_accuracyLIST=c(predVal_accuracyLIST,predVal_cm$overall[["Accuracy"]])
predVal_cutoffLIST=c(predVal_cutoffLIST,val)
}

#Checking for sensitivity and such on fixed cut-off
Tcutoff0.5<- confusionMatrix(as.factor(ifelse(predTrain > 0.5, 1, 0)), as.factor(Train$IsProfitable), positive="1")
Tcutoff0.5
Vcutoff0.5<- confusionMatrix(as.factor(ifelse(predVal > 0.5, 1, 0)), as.factor(Validation$IsProfitable), positive="1")
Vcutoff0.5
#combining accuracy list with cutoff list
predVal_TableOutput<-cbind(data.frame(predVal_cutoffLIST),data.frame(predVal_accuracyLIST))
#output into CSV table
write.csv(predVal_TableOutput,"predVal_CM_Table.csv", row.names = FALSE)
````````````````````````````````````````````````````````````````````
#Define the misclassification costs based on console output from confusion matrix
#1 and 2 are just example numbers
cost.fp = 2 
cost.fn = 1

# create empty error measure tables
MyCost = c()

# compute cost per cutoff
cutoff_range = c(seq(0,1,0.01))
for (val in cutoff_range){
  #finding the confusion matrix for each cutoff
  cm <- confusionMatrix(as.factor(1 * (predVal > val)), as.factor(Validation$IsProfitable), positive = "1")
  #calculate the misclassification cost
  FP = cm$table[2,1]
  FN = cm$table[1,2]
  MyCost = c(MyCost, FP*cost.fp+FN*cost.fn)
}

#plot the misclassification costs
plot(cutoff_range, MyCost, xlab = "CutOff", ylab = "Misclassification Costs",type = "l")
````````````````````````````````````````````````````````````````````````````

##***If you want to create the analysis in Excel, here is the code to export the results:
#add the predictions to the training and testing sets 
#Train<-data.frame(Train, "Predicted" = predTrain)
#Validation<-data.frame(Validation, "Predicted" = predVal)

#add an indicator of whether the data was in the Training or Validation
#Train <- Train %>% mutate(MySample="T")
#Validation <- Validation %>% mutate(MySample="V") 
  
#combine the training and testing
#Credit<-rbind(Train,Validation)

#write the results to a flat file 
#write.csv(Credit,"LRactivityOutput.csv", row.names = FALSE)
``````````````````````````````````````````````````````````````
#Creating ROC curve

#Step0: gather predictions and lables into lists
AllPredictions=list(predTrain,predVal)
AllLabels=list(Train$IsProfitable,Validation$IsProfitable)

#step 1 is to create a "prediction object" that keep the predictions and outcomes
Allpred<-prediction(AllPredictions,AllLabels)

#step 2 we plot:

plot(performance(Allpred,"tpr","fpr"),main="ROC Curve")
abline(0,1)
````````````````````````````````````````````````````````````````````````````````
#Loop is in Excel for calculating profitable NPV
#To create a confusion matrix we first need to get the predictions, then we can create a confusion matrix:
#first we predict the probability of preferring light beer
#colnames(Credit)
rm(list=ls())
setwd("H:/My Drive/BUSM761_DataMining/22SP_BUSM761/Logistic_Regression_Activity")
library(tidyverse)
library(forecast)
library(pacman)
p_load(tidyverse,caret,ROCR)
Credit <- read_csv("credit_dataonly.csv")

Credit$GENDER<-factor(Credit$GENDER)
Credit$CHK_ACCT<-factor(Credit$CHK_ACCT)
Credit$SAV_ACCT<-factor(Credit$SAV_ACCT)
Credit$HISTORY<-factor(Credit$HISTORY)
Credit$PRESENT_RESIDENT<-factor(Credit$PRESENT_RESIDENT)
Credit$EMPLOYMENT<-factor(Credit$EMPLOYMENT)
Credit$JOB<-factor(Credit$JOB)
Credit$TYPE<-factor(Credit$TYPE)
Credit <- Credit %>% mutate (IsProfitable=ifelse(NPV>0,1,0))

set.seed(1)

Train <- Credit %>% sample_frac(0.7) 
Validation <- Credit %>% anti_join(Train, by="OBS") 

lr1NPV <- lm(NPV ~ CHK_ACCT+SAV_ACCT+NUM_CREDITS+DURATION+HISTORY+EMPLOYMENT+OWN_RES+REAL_ESTATE+TYPE, data=Train)
sink("Exhibit4.txt")
summary(lr1NPV)
sink()
step.lr1NPV <- step(lr1NPV,direction="backward",trace=0)
step.lr1NPV$anova
step.lr1NPV$coefficients
NPVpredTrain <- predict(lr1NPV, type="response")
NPVpredVal <- predict(lr1NPV, type="response",newdata = Validation)



#Creating looping range and empty list
NPVcutoff_range = c(seq(1500,from=-1500,by=50))
NPVpredTrain_accuracyLIST=c()
NPVpredTrain_cutoffLIST=c()
NPVpredVal_accuracyLIST=c()
NPVpredVal_cutoffLIST=c()


#looping NPVpredTrain in confusion matrix
for (NPVval in NPVcutoff_range){
  NPVpredTrain_cm <- confusionMatrix(as.factor(ifelse(NPVpredTrain >= NPVval, 1, 0)), as.factor(ifelse(Train$NPV>=NPVval,1,0)), positive="1")
  NPVpredTrain_accuracyLIST=c(NPVpredTrain_accuracyLIST,NPVpredTrain_cm$overall[["Accuracy"]])
  NPVpredTrain_cutoffLIST=c(NPVpredTrain_cutoffLIST,NPVval)

}
#combining accuracy list with cutoff list
NPVpredTrain_TableOutput<-cbind(data.frame(NPVpredTrain_cutoffLIST),data.frame(NPVpredTrain_accuracyLIST))
#output into CSV table
write.csv(NPVpredTrain_TableOutput,"NPVpredTrain_CM_Table.csv", row.names = FALSE)

#looping NPVpredVal in confusion matrix
for (NPVval in NPVcutoff_range){
 NPVpredVal_cm <- confusionMatrix(as.factor(ifelse(NPVpredVal > NPVval, 1, 0)), as.factor(ifelse(Validation$NPV>=NPVval,1,0)), positive="1")
  NPVpredVal_accuracyLIST=c(NPVpredVal_accuracyLIST,NPVpredVal_cm$overall[["Accuracy"]])
  NPVpredVal_cutoffLIST=c(NPVpredVal_cutoffLIST,NPVval)

}
#combining accuracy list with cutoff list
NPVpredVal_TableOutput<-cbind(data.frame(NPVpredVal_cutoffLIST),data.frame(NPVpredVal_accuracyLIST))
#output into CSV table
write.csv(NPVpredVal_TableOutput,"NPVpredVal_CM_Table.csv", row.names = FALSE)

#PLOTTING ROC CURVE
AllPredictions=list(NPVpredTrain,NPVpredVal)
AllLabels=list(Train$IsProfitable,Validation$IsProfitable)

#step 1 is to create a "prediction object" that keep the predictions and outcomes
Allpred<-prediction(AllPredictions,AllLabels)

#step 2 we plot:

plot(performance(Allpred,"tpr","fpr"),main="NPV ROC Curve")
abline(0,1)

# adding labels to indicate T or V
Train <- Train %>% mutate(MySample="T")
Validation <- Validation %>% mutate(MySample="V") 
# predicted NPVs back to the original Credit dataset
Train <- data.frame(Train,"NPV_Predicted" = NPVpredTrain)
Validation <- data.frame(Validation,"NPV_Predicted" = NPVpredVal)
Credit <- rbind(Train,Validation)
write.csv(Credit,"Output_NPV_Cutoff.csv", row.names = FALSE)
