
#imports the file

library(xlsx)
forecastData <- read.xlsx("forecastNewData.xlsx", sheetName="forecastNewData")




#renaming the columnnames

colnames(forecastData)=c("Date","hour","Temperature")

#Transforming the columns
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
forecastData$Date <- as.Date(as.character(forecastData$Date,"%Y%m%d"),"%Y%m%d")
forecastData$Date <- as.Date(forecastData$Date, format = "%m%d%Y")
forecastData$month=format(forecastData$Date, format = "%m")
forecastData$day=format(forecastData$Date, format = "%d")
forecastData$year= format(forecastData$Date, format = "%Y")
forecastData$DayofWeek = weekdays(forecastData$Date, abbreviate = TRUE)
forecastData$Weekday = factor((weekdays(forecastData$Date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c(0, 1))                           
forecastData$Peakhour = with(forecastData, ifelse(hour > 6 & hour < 19, 1, 0))                            
forecastData$DayofWeek <- with(forecastData, ifelse(DayofWeek=="Mon", 1,ifelse(DayofWeek=="Tue", 2,ifelse(DayofWeek=="Wed", 3, ifelse(DayofWeek=="Thu", 4, ifelse(DayofWeek=="Fri", 5, ifelse(DayofWeek=="Sat", 6, ifelse(DayofWeek=="Sun", 0, 7))))))))                            

#sorting the data by Date and hour
forecastData=forecastData[order(forecastData$Date,forecastData$hour),]                           

#rearranging the order of columns 
forecastData=forecastData[,c(1,4,5,6,2,7,8,9,3)]

#To Visualize the outliers for temperature using Box Plot
boxplot(forecastData$Temperature,horizontal = TRUE)
boxplot.stats(forecastData$Temperature)

#Replacing outliers in Temperature with NA by Box Plot
outliers = boxplot(forecastData$Temperature, plot=FALSE)$out
outliers
forecastData[forecastData$Temperature %in% outliers,3]=NA
summary(forecastData)

#Replacing NAs with mean of consecutive 2 observations for temperature

for(i in 1:length(forecastData$Temperature))
{
  if(is.na(forecastData$Temperature[i])==TRUE)
  {
    forecastData$Temperature[i]=mean(forecastData$Temperature[i:(i+2)],na.rm=TRUE)
  }
}
summary(forecastData)

forecastData$day <- as.numeric(forecastData$day)
forecastData$year <- as.numeric(forecastData$year)

forecastData$hour <- as.numeric(forecastData$hour)
forecastData$month <- as.numeric(forecastData$month)


for(i in 1:length(forecastData$day))
{
  if(is.na(forecastData$day[i])==TRUE)
  {
    forecastData$day[i]=format(forecastData$Date[i], format = "%d")
  }
}

forecastData$day <- as.numeric(forecastData$day)

#predict power usage
View(forecastData)



lm.fit <- glm(kWh~.-day -hour, data=train)
summary(lm.fit)

# Predicted data from lm

pr.lm <- predict(lm.fit,test)
summary(pr.lm)
View(sampleformat)

View(forecastData)

forecastData$kWh <- round(predict(pr.nn,forecastData),digits=0)

forecastData$Account <- NULL

forecastData <- forecastData[,c(1,5,9,10)]

colnames(forecastData)=c("Day","Hr","Temp","KWH")

#exporting the file
write.csv(forecastData, "forecastOutput_999999999_regressionTree_newdata.csv",row.names = FALSE)