#Step 1: Write a summarizing function to understand the distribution of a vector
library(moments)
#Creating 'printVecInfo' function
printVecInfo <-function(vectorInputs){
  mean_value <- mean(vectorInputs)
  median_value <- median(vectorInputs)
  min_value <- min(vectorInputs)
  max_value <- max(vectorInputs)
  standard_value <- sd(vectorInputs)
  qt_value <- quantile(vectorInputs, probs = c(0.05, 0.95))
  skw_value <- skewness(vectorInputs)
  cat('Mean : ',mean_value,'\n')
  cat('Median : ',median_value,'\n')
  cat('Min : ',min_value,' ')
  cat('Max : ',max_value, '\n')
  cat('Std : ',standard_value,'\n')
  cat('quantile : ',qt_value,'\n')
  cat('Skewness : ',skw_value,'\n\n')
}
#Assign a vector 'myData'
myData <- c(1,2,3,4,5,6,7,8,9,10,50)
printVectorInfo(myData)
#Step 2: Creating Samples in a Jar 
jar <- c()
jar[1:50] <- "red marble"
jar[51:100] <- "blue marble"
n <- 10
samples <- sample(jar, n, replace=TRUE)
red_prop <- length(which(samples == "red marble"))/n 
red_prop
# The mean to how many reds are in 10 samples 
samples_prop <- replicate(20, length(which((sample(jar,n, replace = TRUE)) == 'red marble'))/n)
# Histogram and printVecInfo
hist(samples_prop)
#stats
printVecInfo(samples_prop)
# sample size= 100; how many red 
num <- 100
samples_prop2 <- replicate(20, length(which((sample(jar,num, replace = TRUE)) == 'red 
marble'))/num)
# Histogram and printVecInfo function 
hist(samples_prop2)
printVecInfo(samples_prop2)
#Step 3: Air quality 
temp <- airquality
summary(temp)
# create a function to remove the NA's 
remove_nas <- function(df, n=0){
  df[rowSums(is.na(df)) <= n,]
  }
temp <- remove_nas(temp)
summary(temp)
#printVecInfo function-Ozone, Wind, and Temp
printVecInfo(temp$Ozone)
printVecInfo(temp$Wind)
printVecInfo(temp$Temp)
#Histogram for Ozone, Wind, and Temp
hist(temp$Ozone)
hist(temp$Wind)
hist(temp$Temp) 