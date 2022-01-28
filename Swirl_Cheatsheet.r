#setwd("directory")
sessionInfo()
rm(list=ls())
#install.packages("swirl",repos = "http://cran.us.r-project.org")
#do this first before running
library(swirl)
swirl()
#basics
#?"any function" = bring up help page
#c(1,2,3, "myname") = a vector containing 1,2,3, myname
#can also be used as the vector of index for later identification
#c("word1" = 1, "word2" = 2) = a vector with named element.
#Sequencing
#1:3 = 1 to 3
#?":" = documentation for colon
#seq(1:20) = 1 to 20
#seq(0,10,by=0.5) = increment by 0.5
#seq(5,10,length=30) = a total of 30 numbers including decimals between 5 and 10
#1:length(my_seq) = from number 1 to length of myseq, which is 30
#seq_along(my_seq) = same function as above, just another built-in R function
#rep(0,times=40) = repeat 0 40 times in a list
#rep(c(0,1,2), times = 10) = repeeat a C vector of (1,2,3) 10 times
#rep(c(0,1,2),each = 10) = 10 of each numbers in sequence (like 0 0 0 1 1 1 2 2 2)
#vectors
#if num_vect(1,-1,2), then num_vect <1 would return vector(FALSE, TRUE, FALSE). It's a conditional statement
#paste(my_char,collapse = " ") = join each words with space in between, all done in the same variable my_char
#c(my_char, "something else") = concatenate both values. Need to assign a variable for storing.
#paste("Hello", "world!", sep = " ") = same thing, except using separation instead of collapsing(gluing)
#paste(1:3, c("X", "Y", "Z"), sep = "") = join 2 vectors of same length, produce 1X, 2Y, 3Z
#paste("x", 1:4, sep = "-") = join letter X to every sequence of number, called recycling
#missing value
#y <- rnorm(1000) = a vector containing 1000 random draws, negative, decimal, positive
#z<- rep(NA,1000) = a vector with NA as the numbers repeated 1000 time
#my_data <- sample(c(y, z), 100) = random samples from c vector taken 100 time
#sum(my_na) = count the number of TRUE value, not FALSE
#my_na <- is.na(my_data) = if a value in my_data is NA, returns TRUE, otherwise FALSE
#subsetting vectors
#x[is.na(x)] = return x vector values that are NA
#x[!is.na(x)] = return x vector values that are NOT NA
#x[x>0] = return x vector values that are greater than 0 AND NAs because NA is a placeholder
#x[!is.na(x) & x > 0] = return x vector values that are greater than 0 ONLY
#x[c(2,10)] = return indexed elements in 2nd and 10th index.
#x[-c(2, 10)] or x[c(-2, -10)] = return indexed elements that are NOT 2nd and 10th
#names("named vector") = return the names of the elements in the vector
#names("unnamed vector") <- c("foo", "bar", "norf") = manual way to create a named vector
#"named vector"["name"] = return the element under the name index
#"named vector"[c("foo", "bar")] = return the element under the SPECIFIED name index
#matrices and data frame
#dim(my_vector) <- c(4, 5) = give dim attribute of 4 rows and 5 columns to my_vector, turns it into matrix
#attribute(my_vector) = checks the attribute; first number is ROW, second number is COLUMN
#class(my_vector) = check for class
#my_matrix2 <- matrix(1:20,nrow=4,ncol = 5,byrow = FALSE,dimnames = NULL)
#creating matrix in 1 step instead of multiple dim attribute
#identical("variable1","variable2",so on) = return TRUE if they are identical, else FALSE
#cbind(variable1","variable2",so on) = combine vectors with matrix in column
#my_data <- data.frame(patients, my_matrix) = loading vectors and matrix into data frame
#also remove the char num conflict by forming a single object of class called data.frame
#colnames(my_data) <- cnames = assign column name to my_data from cnames vector
#logic
#single sign applies to the whole vector
#double sign only apply to the 1st element of a vector
#TRUE take precedence over FALSE in OR function
#OR: TRUE TRUE return FALSE, FALSE FALSE return FALSE
#OR: TRUE FALSE or FALSE TRUE return TRUE
#isTRUE("1 argument") = return if argument is TRUE or not.
#xor("argument1","argument2") = return TRUE of 1 OR 2 is TRUE
#which("argument of a vector") = returns index of vector that is TRUE
#any("argument of a vector") = returns TRUE if 1 or more element is TRUE to argument
#all("argument of a vector") = returns TRUE if all elements are TRUE to argument
#functions
#sys.date()
#"functionX" <- function("input1", "default input2 = somthin" **2nd input can be changed when called**) {
#code between the {}
#} close bracket
#you can also call "functionX"(input1 = somthing, input2 = somthing)
#args(functionX) = see what the function structure looks like
#functionX(function(input1name){outcome of input1name}, input1)
#the function() return whatever is in the {} in that format.
#mad_libs <- function(...){
#STEP1: turn ellipse into list-> args <- list(...)
#STEP2: assign names to args list function:-> place <- args[["place"]]
#STEP2: assign names to args list function:->adjective <- args[["adjective"]]
#STEP2: assign names to args list function:->noun <- args[["noun"]]
#STEP3: layout the paste format:-> paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")
#}
#looking at data
#ls() = see existing datasets (lists)
#class("variable1") = check data type
#nrow("variable1")
#ncol("variable2")
#object.size(plants) = tells how much memory the file eat
#names("variable1") = check column headers
#head("variable") = default view of first 6 rows
#tail("variable1") = default view of last 6 rows
#str("variable1") = check category type
#plot
#plot("variable1") = plot the datasets
#plot(x = "variable1cat1", y = "variable1cat2", xlab = "labelname", ylab = "labelname") = adding categories to x and y axis
#plot(variable$categoryForXaxis,variable$categoryForYaxis ) = same as other ploting
#plot("variable1", main = "xx") = give main title
#plot("variable1", sub = "xx") = give sub title
#plot("variable1", col = "a number") = give color 
#plot("variable1", pch = "a number") = give plot shape
#plot("variable1", xlim = c(10, 15)) = limit xaxis to display from unit 10 to 15th index
#boxplot() = box and whisker chart
#hist() = histogram