# install packages 
install.packages("RCurl")
install.packages("jsonlite")
library(jsonlite)
library(RCurl)
library(sqldf)
# load the dataset
JsonURL <-"http://opendata.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
#store dataset into a vector, convert into JSON format, and drop first eight columns
apiResult <- getURL(JsonURL)
Jsondata <- jsonlite::from JSON(apiResult)
dfResult <- data.frame(Jsondata[[2]])
#Rename the columns 
nameList <-
  c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WE
EK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NA
ME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLI
SION_WITH_1","COLLISION_WITH_2")
cleandf <- function(df,namesOfColumns){
  df <- df[,-(1:8)] 
  colnames(df) <- namesOfColumns
  return (df)
}
df <- cleandf(dfResult,namesofColumns)
str(df)
#Create a function to remove na's 
remove_na <- function(df,n=0){
  df <- df[rowSums(is.na(df)) <= n,]
  df <- as.data.frame.matrix(df)
  return (df)
}
df <-remove_na(df)
str(df)
#change the datatype of ACC_DATE, ACC_TIME, VEHICLE_COUNT to date, numeric, and integer
library(chron)
df$ACC_DATE <- as.Date(df$ACC_DATE)
df$ACC_TIME <- as.numeric(df$ACC_TIME)
df$VEHICLE_COUNT <- as.integer(df$VEHICLE_COUNT)
#Remove space in Day_of_Week
df$DAY_OF_WEEK <- gsub(" ","",df$DAY_OF_WEEK)
#Import SQLDF and solve for how many accidents happened on Sunday
sunday_acct <- sqldf(" select count(DAY_OF_WEEK), DAY_OF_WEEK from df where DAY_OF_WEEK = 
'SUNDAY' ")
sunday_acct
#How many injuries 
sqldf("select count(*) as numOfAccidentWithInjuries from df where INJURY='YES'")
#List of injuries per day 
injurybyday <- sqldf(" select DAY_OF_WEEK, count(DAY_OF_WEEK) as numofaccident from df where 
INJURY='YES' group by DAY_OF_WEEK")
injurybyday
#Using tapply, how many accidents happen on Sunday
tapply(df$CASE_NUMBER, df$DAY_OF_WEEK=='SUNDAY', length)
#How many accidents had injuries 
tapply(df$CASE_NUMBER, df$INJURY == 'YES', length)
#List of injuries 
injuries <- df[which(df$INJURY == 'YES'),]
tapply(injuries$CASE_NUMBER, injuries$DAY_OF_WEEK,length)


      








