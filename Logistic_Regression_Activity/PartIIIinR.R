#Define the misclassification costs
cost.fp = 2 
cost.fn = 1

# create empty error measure tables
MyCost = c()

# compute cost per cutoff
for (cut in seq(0,1,0.01)){
  #finding the confusion matrix for each cutoff
  cm <- confusionMatrix(as.factor(1 * (predprob > cut)), as.factor(Beer$Preference), positive = "1")
  #calculate the misclassification cost
  FP = cm$table[2,1]
  FN = cm$table[1,2]
  MyCost = c(MyCost, FP*cost.fp+FN*cost.fn)
}

#plot the misclassification costs
plot(seq(0,1,0.01), MyCost, xlab = "CutOff", ylab = "Misclassification Costs",type = "l")
