library("TTR")
library('MASS')


setwd("C:/Users/arakumar/Desktop/RedHatSharedFolder/walmart_labs")
trainMerged <- read.csv("trainMerged.csv")
#train <- na.omit(train_data)
testMerged <- read.csv('testMerged.csv')
max(trainMerged$Dept)
Stores = c(1:45)
Dept = c(1:99)
Store.Dept <- expand.grid(x = Stores, y = Dept)

getData <- function(x , y){
  dat1 <- subset(trainMerged, subset = (Store == x & Dept ==y ) , select =c(Date, Weekly_Sales,IsHoliday, Type, Size, Temperature, Fuel_Price, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, CPI, Unemployment))                                                                          
  return (dat1)
}


for (i in 1:dim(Store.Dept)[1]){
  dat <- getData (Store.Dept[i, 1], Store.Dept[i, 2])
}

AllDates <- as.data.frame(unique(trainMerged$Date))
names(AllDates) = c('Date')

Store_Dept_data <- mapply ( getData , Store.Dept[,1], Store.Dept[,2] )

impute <- function(Store_Dept_data, Store.Dept, AllDates){
  dfstore_dep <- data.frame()
  df <- data.frame(1:143)
  Weekly_Sales   <- data.frame(1:143)
  IsHoliday    	<- data.frame(1:143)
  Type         	<- data.frame(1:143)
  Size         	<- data.frame(1:143)
  Temperature  	<- data.frame(1:143)
  Fuel_Price   	<- data.frame(1:143)
  MarkDown1    	<- data.frame(1:143)
  MarkDown2    	<- data.frame(1:143)
  MarkDown3    	<- data.frame(1:143)
  MarkDown4    	<- data.frame(1:143)
  MarkDown5    	<- data.frame(1:143)
  CPI          	<- data.frame(1:143)
  
  Unemployment 	<- data.frame(1:143)
  
  numStoreDepEntries <- dim(Store_Dept_data)[2]
  for (i in 1:numStoreDepEntries){
    
    store_dep_data <- (as.data.frame(Store_Dept_data[,i]))
    dfstore_dep <- merge(x = AllDates, y = store_dep_data, all.x= TRUE)
    Weekly_Sales[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Weekly_Sales
    IsHoliday[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$IsHoliday
    Type[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Type
    Size[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Size
    Temperature[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Temperature
    Fuel_Price[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Fuel_Price
    CPI[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$CPI
    Unemployment[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$Unemployment
    MarkDown1  	[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$MarkDown1
    MarkDown2		[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$MarkDown2
    MarkDown3 [ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$MarkDown3
    MarkDown4  [ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$MarkDown4
    MarkDown5			[ , paste(Store.Dept[i,], collapse = "_")] <- dfstore_dep$MarkDown5
   
    
  }
  mylist <- list(df, Weekly_Sales, IsHoliday, Type, Size, Temperature, Fuel_Price, CPI, Unemployment, MarkDown1, MarkDown2, MarkDown3,MarkDown4, MarkDown5)
  return (mylist)
}

ImputatedData <- impute(Store_Dept_data, Store.Dept, AllDates)

write.csv(ImputatedData[[1]], file='df.csv')
write.csv(ImputatedData[[2]], file='Weekly_Sales.csv')
write.csv(ImputatedData[[3]], file='IsHoliday.csv')
write.csv(ImputatedData[[4]], file='type.csv')
write.csv(ImputatedData[[5]], file='Size.csv')
write.csv(ImputatedData[[6]], file='Temperature.csv')
write.csv(ImputatedData[[7]], file='Fuel_Price.csv')
write.csv(ImputatedData[[8]], file='CPI.csv')
write.csv(ImputatedData[[9]], file='Unemployment.csv')
write.csv(ImputatedData[[10]], file='MarkDown1.csv')
write.csv(ImputatedData[[11]], file='MarkDown2.csv')
write.csv(ImputatedData[[12]], file='MarkDown3.csv')
write.csv(ImputatedData[[13]], file='MarkDown4.csv')
write.csv(ImputatedData[[14]], file='MarkDown5.csv')


Weekly_Sales <- read.csv('Weekly_Sales.csv')
Weekly_Sales[is.na(Weekly_Sales)] <- 0

getSeasonalComponent <- function(ImputatedData){
  #if(length(unlist(outcome)) != 143) return (rep(0,143))
  cat("hello")
  df <- data.frame(1:143)
  
  for ( i in 2:dim(ImputatedData)[2] ){
    walmarttimeseries <- ts(ImputatedData[,i], frequency = 52, start = c(2011, 1))
    walmarttimeseriescomponents <- decompose(walmarttimeseries)
    df[ , paste(names(ImputatedData)[i])] <- walmarttimeseriescomponents$seasonal
  }
  return (df)
}

#Fill up the NA's in Weekly_Sales with zeros for now
SeasonalComponent <-getSeasonalComponent(Weekly_Sales)


remainingTS <- function (Weekly_Sales, SeasonalComponent){
  df <- as.data.frame(1:143)
  
  for (i in 2:dim(SeasonalComponent)[2]){
    residualTS <- (Weekly_Sales[,i-1]) - SeasonalComponent[,i]
    df[ , paste(names(SeasonalComponent)[i])] <- residualTS
  }
  return (df)
}


SeasonalAdjusted <- remainingTS(Weekly_Sales, SeasonalComponent)
write.csv(SeasonalComponent, file='SeasonalComponent.csv')
write.csv(SeasonalAdjusted, file = 'SeasonalAdjusted.csv')




testDates <- as.data.frame(unique(testMerged$Date))
names(testDates) = c('Date')



d<- seq(as.Date("2010-01-08"), as.Date("2013-12-31"), "week") 
k <- c(1 : length(d))
k <- k %% 52
k <- ifelse(k == 0, 52, k)
holiday_weeknumber = data.frame(d, k)
names(holiday_weeknumber) <- c('week_name','week_number')


