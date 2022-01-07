setwd("~/Local_Repo/22SP_BUSM761/22SP_BUSM761")
print("Hello world", quote = FALSE)
sessionInfo()
#session check cleanspace

#install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#library(ggplot2)
#sessionInfo()
#updated session check for installed package

#housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
#summary(housing.df)

rm(list=ls())
install.packages("swirl",repos = "http://cran.us.r-project.org")
library(swirl)
swirl()