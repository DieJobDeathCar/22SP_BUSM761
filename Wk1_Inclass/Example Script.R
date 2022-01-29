#load packages
#install.packages("tidyverse")
library(tidyverse) 

# read data
Credit <- read_csv("credit_dataonly.csv")

#######################################
#Plot histogram of NPV: With basic R
######################################

#basic
hist(Credit$NPV)

#add good labels
hist(Credit$NPV, xlab="NPV", main="Starting my exploration with a histogram")

#add color and play with the y-axis
hist(Credit$NPV, xlab="NPV", main="We can add color and change to density", col="darkmagenta", freq=FALSE)

#add labels
h <- hist(Credit$NPV, xlab="NPV", main="Now lets add some labels")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

#######################################
#Plot a histogram of NPV: With ggplot
#######################################

#basic
ggplot(Credit, aes(x=NPV)) + geom_histogram()


#add titles
ggplot(Credit, aes(x=NPV)) + geom_histogram() + 
  labs(title="NPV histogram plot",x="NPV($)", y = "Count")

#play with colors
ggplot(Credit, aes(x=NPV)) + geom_histogram(color="black", fill="white") + 
  labs(title="NPV histogram plot",x="NPV($)", y = "Count")


#add the mean line
ggplot(Credit, aes(x=NPV)) + geom_histogram(color="black", fill="white") + 
  labs(title="NPV histogram plot",x="NPV($)", y = "Count")+
  geom_vline(aes(xintercept=mean(NPV)), color="blue",linetype="dashed")

#add the third dimension thought color, plot both and note the difference
# Change histogram plot line colors by groups
ggplot(Credit, aes(x=NPV, color=GENDER)) +  geom_histogram(fill="white") + 
  labs(title="NPV histogram plot",x="NPV($)", y = "Count")

#overlaid histograms
ggplot(Credit, aes(x=NPV, color=GENDER)) +  geom_histogram(fill="white", alpha=0.5, position="identity")+
   labs(title="NPV histogram plot",x="NPV($)", y = "Count")

#next to each other - and changing the color scheme
ggplot(Credit, aes(x=NPV, color=GENDER)) +  geom_histogram(fill="white", alpha=0.5, position="dodge") + 
  labs(title="NPV histogram plot",x="NPV($)", y = "Count")+
  scale_color_brewer(palette="Dark2")

#check out the ggplot cheat sheet: 

#######################################
#Plot a scatterplot using plot()
#######################################

#simple scatter plot
plot(Credit$AGE,Credit$NPV)

#ad labels and title
plot(Credit$AGE,Credit$NPV,
     main="Age vs NPV",
     xlab="Age",
     ylab="NPV ($)")

#add colors and shape
plot(Credit$AGE,Credit$NPV, col="deepskyblue", pch=8, 
     main="Age vs NPV",
     xlab="Age",
     ylab="NPV ($)")


#color by group
plot(Credit$AGE,Credit$NPV,col=c("red","blue")[as.factor(Credit$GENDER)],
     main="Age vs NPV",
     xlab="Age",
     ylab="NPV ($)")
legend(x="bottomright", legend = levels(as.factor(Credit$GENDER)), col=c("red","blue"), pch=1, bty="n")


#######################################
#Plot a scatterplot using ggplot()
#######################################

#basic
ggplot(Credit, aes(x=AGE, y=NPV)) + geom_point()

#add labels and title
ggplot(Credit, aes(x=AGE, y=NPV)) + geom_point() +
labs(title="My first ggplot() scatter plot",x="AGE", y="NPV($)")

#use color as the third dimension
ggplot(Credit, aes(x=AGE, y=NPV, color=as.factor(GENDER))) + geom_point() +
  labs(title="My first ggplot() scatter plot",x="AGE", y="NPV($)",color="Gender")


#change it into a bubble plot with size
ggplot(Credit, aes(x=AGE, y=NPV, size=as.factor(NUM_DEPENDENTS), color=as.factor(GENDER))) + geom_point() +
  labs(
    title="This is not a good plot",
    subtitle = "But it demonstrates the use of color, size and subtitles",
    x="AGE", y="NPV($)",size="# dependents",color="Gender")
  

#######################################
#Plot a matrix plot of AGE, CHK_ACCT, SAV_ACCT and NPV
#######################################
#for ease of coding select the columns we want to plot
plotdata<-Credit %>% select(AGE,CHK_ACCT,SAV_ACCT,NPV)
#plot!
plot(plotdata)

#adding jitter (and using ggplot)
ggplot(Credit, aes(x=CHK_ACCT, y=SAV_ACCT))+
    geom_jitter(width = 0.2, height = 0.2)

#######################################
#Boxplot in basic R
#######################################

#basic boxplots
boxplot(Credit$NPV)
boxplot(Credit$NPV~as.factor(Credit$TYPE))

#Your turn: use what you have learned to add text and colors
#hint: col = c(list your favorite colors))

#######################################
#Boxplot in ggplot()
#######################################

# Basic box plot
ggplot(Credit, aes(x=as.factor(TYPE), y=NPV)) + 
  geom_boxplot()

# go crazy with the options
ggplot(Credit, aes(x=as.factor(TYPE), y=NPV)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

#your turn: add text colors, google how to add the average!


#############################################
############ PART II ########################
############################################

#add the forecast package for easy error measures
library(forecast)

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