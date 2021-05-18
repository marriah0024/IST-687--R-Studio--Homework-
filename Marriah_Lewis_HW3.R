#Read in the URL
urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
#Store as a dataset into a dataframe
dfStates <- read.csv(url(urlToRead))
str(dfStates)
# Removing columns and removing rows
dfStates <- dfStates[-1:-8,]
dfStates <- dfStates[,1:5]
tail(dfStates, 5) 
dfStates <- dfStates[-52:-58,]
dfStates$stateName <- dfStates[,1]
colnames(dfStates)
dfStates <- dfStates[,-1]
colnames(dfStates)
#Removing commas
dfStates$stateName <- gsub("\\.","",dfStates$stateName)
dfStates$base2010 <-gsub(",","",dfStates$X)
dfStates$base2011 <-gsub(",","",dfStates$X.1)
dfStates$Jul2010 <-gsub(",","", dfStates$X.2)
dfStates$Jul2011 <-gsub(",","", dfStates$X.3)
#Removing space and convert to numbers
dfStates$base2010 <- as.numeric(gsub("","",dfStates$base2010))
dfStates$base2011 <- as.numeric(gsub("","",dfStates$base2011))
dfStates$Jul2010 <- as.numeric(gsub("","", dfStates$Jul2010))
dfStates$Jul2011 <- as.numeric(gsub("", "", dfStates$Jul2011))
#Removing the X names
dfStates <- dfStates[, -1:-4]
str(dfStates)
#Calculating the mean for the July 2011 data 
mean(dfStates$Jul2011)
#Population of the state with the highest population and the name of the state
index <- which.max(dfStates$Jul2011)
dfStates[index, ]
max(dfStates$Jul2011)
#Sort the data, in increasing order, based on the July 2011 data
rownames(dfStates) <- NULL
sortedStates <- dfStates[order(-dfStates$Jul2011),]
head(sortedStates,5)
# Assign the dfStates$Jul2011 to myVector
myVector<-dfStates$Jul2011
#Solve for the length 
len<-length(myVector)
#Assign the mean of dfStates$Jul2011 to me_July
me_July<- mean(dfStates$Jul2011)
# function will return the percentage of the elements within the vector that is less than the same
Vector2 <-myVector[myVector< me_July]
len_2<-length(Vector2)
percentage<-(len_2/len)*100
percentage