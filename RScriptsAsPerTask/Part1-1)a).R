#NOTE: uncomment the lines from 6 to 12 if the packages are not installed and loaded
#      in your system. Installing and loading the libraries everytime you run the
#      program makes the execution time longer. Hence commented those parts.

#installing and importing the library
install.packages('stringr')
library(stringr)
install.packages('weatherData')
library(weatherData)
install.packages('outliers')
library(outliers)

#helps the user to pick the file
x=file.choose(new = FALSE)
 
#importing the file
NewData<-read.csv(x, header = TRUE)

#removing the unecessary columns
NewData$Channel=NULL

#removing the blank spaces
NewData$Units=str_replace_all(NewData$Units, " ", ".")

#removing the unnecesary records
NewData <- subset(NewData, Units=="kWh")

#reshaping the dataframe wide to long 
NewData$Units=NULL
NewData <- reshape(NewData, 
             varying = c(colnames(NewData[,-1:-2])), 
             v.names = "kWh",
             timevar = "hour", 
             times = sort(c(rep(seq(from=0,to=23,by=1),12))), 
             direction = "long")

# Forming a Subset
Account=NewData[,1]

#Sorting the data frame by Date
NewData=NewData[order(NewData$Date),]

#To Visualize the outliers for kwh using Box Plot
boxplot(NewData$kWh,horizontal = TRUE)
boxplot.stats(NewData$kWh)

#Replacing outliers with NA by Box Plot
outliers = boxplot(NewData$kWh, plot=FALSE)$out
outliers
NewData[NewData$kWh %in% outliers,3]=NA
summary(NewData)

#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(NewData$kWh))
{
  if(is.na(NewData$kWh[i])==TRUE)
  {
    NewData$kWh[i]=mean(NewData$kWh[i:(i+2)],na.rm=TRUE)
  }
}
summary(NewData)

#aggregate by Date and hour to find hourly kWh
NewData=aggregate(NewData$kWh,
          list(Date = NewData$Date, hour = NewData$hour),
          sum)

#renaming the columns
colnames(NewData)=c("Date","hour","kWh")

#Adding the column Account
NewData$Account=c(rep(Account[1],length(nrow(NewData))))

#Formatting Date column and Adding day,month, year, Dayofweek, weekday and Peakhour Columns
NewData$Date <- as.Date(NewData$Date, format="%m/%d/%Y")
NewData$month=format(NewData$Date, format = "%m")
NewData$day=format(NewData$Date, format = "%d")
NewData$year= format(NewData$Date, format = "%Y")
NewData$DayofWeek = weekdays(NewData$Date, abbreviate = TRUE)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
NewData$Weekday = factor((weekdays(NewData$Date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c(0, 1))                           
NewData$Peakhour = with(NewData, ifelse(hour > 6 & hour < 19, 1, 0))                            

#Sorting the data frame by Date and hour column
NewData=NewData[order(NewData$Date,NewData$hour),]                           

NewData$DayofWeek <- with(NewData, ifelse(DayofWeek=="Mon", 1,ifelse(DayofWeek=="Tue", 2,ifelse(DayofWeek=="Wed", 3, ifelse(DayofWeek=="Thu", 4, ifelse(DayofWeek=="Fri", 5, ifelse(DayofWeek=="Sat", 6, ifelse(DayofWeek=="Sun", 0,7))))))))

#rearranging the order of columns 
NewData=NewData[,c(4,1,3,5,6,7,2,8,9,10)]


#getting the hourly temperature data using R package
#weather <- getWeatherForDate("KBOS", NewData$Date[1], end_date=NewData$Date[length(NewData$Date)], opt_detailed = TRUE, opt_all_columns = TRUE)

#write.csv(weather,"Boston Weather.csv",row.names=FALSE)

bon=file.choose(new = FALSE)
weather<-read.csv(bon, header = TRUE)


#Binning and removing unnecessary columns
weather=weather[,c(1,3)]

#renaming the columns and Dealing with Date Time format
colnames(weather)=c("Date","Temperature")
weather$Date=as.POSIXct(weather$Date, tz="America/New_York")
weather$hour=format(weather$Date,format="%H")
weather$hour=as.numeric(weather$hour)
weather$Date=as.Date(as.character(as.POSIXct(weather$Date, tz="America/New_York")))

#Transforming the temperature data 

summary(weather)
weather=weather[,c(1,3,2)]
summary(weather)

weather=weather[order(weather$Date,weather$hour),]

#To Visualize the outliers for temperature using Box Plot
boxplot(weather$Temperature,horizontal = TRUE)
boxplot.stats(weather$Temperature)

#Replacing outliers with NA by Box Plot
outliers = boxplot(weather$Temperature, plot=FALSE)$out
outliers
weather[weather$Temperature %in% outliers,3]=NA
summary(weather)

#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(weather$Temperature))
{
  if(is.na(weather$Temperature[i])==TRUE)
  {
    weather$Temperature[i]=mean(weather$Temperature[i:(i+2)],na.rm=TRUE)
  }
}
summary(weather)

#aggregating temperature by Date and hour
weather=weather[order(weather$Date,weather$hour),]
weather=aggregate(weather$Temperature,
                       list(Date = weather$Date, hour = weather$hour),
                       mean)
summary(weather)

#renaming the columuns after aggregation
colnames(weather)=c("Date","hour","Temperature")
colnames(weather)

# rounding the decimal points in Temperature
weather$Temperature <- round(weather$Temperature,digits=0)

#merging the two data frames by left outer join
sampleformat=merge(NewData, weather,by=c("Date","hour"),all.x=TRUE)
summary(sampleformat)

#rearranging the order of columns for the desired output
sampleformat=sampleformat[,c(3,1,4,5,6,7,2,8,9,10,11)]

#Sorting the merged data by Date and hour
sampleformat=sampleformat[order(sampleformat$Date,sampleformat$hour),]

#To Visualize the outliers for merged data using Box Plot
boxplot(sampleformat$Temperature,horizontal = TRUE)
boxplot.stats(sampleformat$Temperature)

#checking for outliers in merged data and replacing them with NA
outliers = boxplot(sampleformat$Temperature, plot=FALSE)$out
outliers
sampleformat[sampleformat$Temperature %in% outliers,3]=NA
summary(sampleformat)

#checking for NA's in merged data and replacing them with mean of 2 consecutive observations 
for(i in 1:length(sampleformat$Temperature))
{
  if(is.na(sampleformat$Temperature[i])==TRUE)
  {
    sampleformat$Temperature[i]=mean(sampleformat$Temperature[i:(i+2)],na.rm=TRUE)
  }
}
for(i in 1:length(sampleformat$Temperature))
{
  if(is.na(sampleformat$Temperature[i])==TRUE)
  {
    sampleformat$Temperature[i]=mean(sampleformat$Temperature[i:(i+2)],na.rm=TRUE)
  }
}

#Replacing NAs with mean of next 2 observations for kwh
for(i in 1:length(sampleformat$kWh))
{
  if(is.na(sampleformat$kWh[i])==TRUE)
  {
    sampleformat$kWh[i]=mean(sampleformat$kWh[i:(i+2)],na.rm=TRUE)
  }
}

summary(sampleformat)

#rounding the decimal values in Temperature
sampleformat$Temperature <- round(sampleformat$Temperature,digits=0)

sampleformat$month <- as.numeric(sampleformat$month)
sampleformat$day <- as.numeric(sampleformat$day)
sampleformat$year <- as.numeric(sampleformat$year)
#sampleformat$Weekday <- as.numeric(sampleformat$Weekday)

sampleformatnozero<-sampleformat[!(sampleformat$kWh==0),]

summary(sampleformat)

install.packages('forecast')

############################################Start Regression for Data with zeros########################################

sampleformat$DayofWeek <- as.factor(sampleformat$DayofWeek)

#singularities for account and year so remove them.
#lmzero.fit=lm(kWh~. -Account -Date -day -year -Weekday, data=sampleformat)
#summary(lmzero.fit)

library(MASS)
library(ISLR)
smp_size <- floor(0.80*nrow(sampleformat))
set.seed(123)
trainzero_ind <- sample(seq_len(nrow(sampleformat)),size=smp_size)
trainzero <- sampleformat[trainzero_ind, ]
testzero <- sampleformat[-trainzero_ind, ]

lmzero.fit = lm(kWh~. -Account -Date -day -year -Weekday, data = trainzero)
summary(lmzero.fit)

library(forecast)
predzero = predict(lmzero.fit, testzero)

#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,train$kWh)
accuracy(predzero,trainzero$kWh)
b= lmzero.fit$coefficients

write.csv(b, "RegressionOutputs(1abc.b).csv")
write.csv(a, "PerformanceMatrics(1abc.b).csv")

#############################################Regression without zeros###################################################

sampleformatnozero$DayofWeek <- as.numeric(sampleformatnozero$DayofWeek)

#singularities for account and year so remove them.
#lm.fit=lm(kWh~. -Account -Date -day -hour -year, data=sampleformatnozero)
#summary(lm.fit)

library(MASS)
library(ISLR)
smp_size <- floor(0.80*nrow(sampleformatnozero))
set.seed(123)
train_ind <- sample(seq_len(nrow(sampleformatnozero)),size=smp_size)
train <- sampleformatnozero[train_ind, ]
test <- sampleformatnozero[-train_ind, ]

lm.fit = lm(kWh~. - Account - year - hour, data = train)
summary(lm.fit)

library(forecast)
pred = predict(lm.fit, test)

#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,train$kWh)
accuracy(pred,train$kWh)
b= lm.fit$coefficients

write.csv(b, "RegressionOutputs(1a.a).csv")
write.csv(a, "PerformanceMatrics(1a.a).csv")