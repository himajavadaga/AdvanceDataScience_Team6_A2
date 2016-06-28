#NOTE: uncomment the lines from 6 to 12 if the packages are not installed and loaded
#      in your system. Installing and loading the libraries everytime you run the
#      program makes the execution time longer. Hence commented those parts.

#installing and importing the library
#install.packages('stringr')
library(stringr)
#install.packages('weatherData')
library(weatherData)
#install.packages('outliers')
install.packages('zoo')
library(outliers)
library(zoo)

#helps the user to pick the file
x=file.choose(new = FALSE)

#importing the file
rawdata<-read.csv(x, header = TRUE)

#removing the unecessary columns
rawdata$Channel=NULL

#removing the blank spaces
rawdata$Units=str_replace_all(rawdata$Units, " ", ".")

#removing the unnecesary records
rawdata <- subset(rawdata, Units=="kWh")

#reshaping the dataframe wide to long 
rawdata$Units=NULL
rawdata <- reshape(rawdata, 
                   varying = c(colnames(rawdata[,-1:-2])), 
                   v.names = "kWh",
                   timevar = "hour", 
                   times = sort(c(rep(seq(from=0,to=23,by=1),12))), 
                   direction = "long")

# Forming a Subset
Account=rawdata[,1]

#Sorting the data frame by Date
rawdata=rawdata[order(rawdata$Date),]

#To Visualize the outliers for kwh using Box Plot
boxplot(rawdata$kWh,horizontal = TRUE)
boxplot.stats(rawdata$kWh)

#Replacing outliers with NA by Box Plot
outliers = boxplot(rawdata$kWh, plot=FALSE)$out
outliers
rawdata[rawdata$kWh %in% outliers,3]=NA
summary(rawdata)

#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(rawdata$kWh))
{
  if(is.na(rawdata$kWh[i])==TRUE)
  {
    rawdata$kWh[i]=mean(rawdata$kWh[i:(i+2)],na.rm=TRUE)
  }
}
summary(rawdata)

#aggregate by Date and hour to find hourly kWh
rawdata=aggregate(rawdata$kWh,
                  list(Date = rawdata$Date, hour = rawdata$hour),
                  sum)

#renaming the columns
colnames(rawdata)=c("Date","hour","kWh")

#Adding the column Account
rawdata$Account=c(rep(Account[1],length(nrow(rawdata))))

#Formatting Date column and Adding day,month, year, Dayofweek, weekday and Peakhour Columns
rawdata$Date <- as.Date(rawdata$Date, format="%m/%d/%Y")
rawdata$month=format(rawdata$Date, format = "%m")
rawdata$day=format(rawdata$Date, format = "%d")
rawdata$year= format(rawdata$Date, format = "%Y")
rawdata$DayofWeek = weekdays(rawdata$Date, abbreviate = TRUE)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
rawdata$Weekday = factor((weekdays(rawdata$Date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c(0, 1))                           
rawdata$Peakhour = with(rawdata, ifelse(hour > 6 & hour < 19, 1, 0))                            

#Sorting the data frame by Date and hour column
rawdata=rawdata[order(rawdata$Date,rawdata$hour),]                           

rawdata$DayofWeek <- with(rawdata, ifelse(DayofWeek=="Mon", 1,ifelse(DayofWeek=="Tue", 2,ifelse(DayofWeek=="Wed", 3, ifelse(DayofWeek=="Thu", 4, ifelse(DayofWeek=="Fri", 5, ifelse(DayofWeek=="Sat", 6, ifelse(DayofWeek=="Sun", 0,7))))))))

#rearranging the order of columns 
rawdata=rawdata[,c(4,1,3,5,6,7,2,8,9,10)]


summary(rawdata)


#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(rawdata$kWh))
{
  if(is.na(rawdata$kWh[i])==TRUE)
  {
    rawdata$kWh[i]=mean(rawdata$kWh[i:(i+2)],na.rm=TRUE)
  }
}
summary(rawdata)


#replacing zeros with NA

for(i in 1:length(rawdata$kWh))
{
  if(rawdata$kWh[i] == 0)
  {
    rawdata$kWh[i]=NA
  }
}
summary(rawdata)


#Using Zoo package and filling NA's
rawdata$kWh <- zoo(rawdata$kWh)
rawdata$kWh=na.fill(rawdata$kWh, "extend")
summary(rawdata)
#write.csv(rawdata,"New Data without Zeros.csv",row.names = FALSE)

bon=file.choose(new = FALSE)
weather<-read.csv(bon, header = TRUE)
#weather <- getWeatherForDate("KBOS", rawdata$Date[1], end_date=rawdata$Date[length(rawdata$Date)], opt_detailed = TRUE, opt_all_columns = TRUE)

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
summary(weather)
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
sampleformat=merge(rawdata, weather,by=c("Date","hour"),all.x=TRUE)
summary(sampleformat)

#rearranging the order of columns for the desired output
sampleformat=sampleformat[,c(3,1,4,5,6,7,2,8,9,10,11)]

#Sorting the merged data by Date and hour
sampleformat=sampleformat[order(sampleformat$Date,sampleformat$hour),]

#To Visualize the outliers for merged data using Box Plot
boxplot(sampleformat$Temperature,horizontal = TRUE)
boxplot.stats(sampleformat$Temperature)
summary(sampleformat)

#checking for outliers in merged data and replacing them with NA
outliers = boxplot(sampleformat$Temperature, plot=FALSE)$out
outliers
sampleformat[sampleformat$Temperature %in% outliers,11]=NA
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

summary(sampleformat)
write.csv(sampleformat, "Hourly_filled_data.csv",row.names = FALSE)

####REGRESSION###


#Start Regression
#install.packages('forecast')
#lm.fit=lm(kWh~., data=sampleformat)

#singularities for account and year so remove them.

sampleformat$kWh <- as.numeric(sampleformat$kWh)
library(MASS)
library(ISLR)
smp_size <- floor(0.80*nrow(sampleformat))
set.seed(123)
train_ind <- sample(seq_len(nrow(sampleformat)),size=smp_size)
train <- sampleformat[train_ind, ]
test <- sampleformat[-train_ind, ]

lm.fit1= lm(kWh~.-Account -year, data = train)
summary(lm.fit1)

library(forecast)
pred = predict(lm.fit1, test)

#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,test$kWh)
a
b= lm.fit1$coefficients
b
write.csv(b, "RegressionOutputs(1c).csv")
write.csv(a, "PerformanceMatrics(1c).csv")
summary(sampleformat)



####2.PREDICTION


#install.packages('tree')
library (tree)
library (MASS)
library (ISLR)
set.seed (1)
train = sample (1:nrow(sampleformat), nrow(sampleformat)/2)
tree.sf = tree(kWh~.-Account -Date -year -day -hour,sampleformat,subset=train)
summary (tree.sf)
plot (tree.sf)
text (tree.sf, pretty = 0)
cv.sf = cv.tree (tree.sf)
plot (cv.sf$size, cv.sf$dev, type='b')

prune.sf =prune.tree(tree.sf, best = 5)
#regression tree model output
plot(prune.sf)
text(prune.sf, pretty = 0)

yhat=predict (tree.sf, newdata =sampleformat [-train,])
sf.test=sampleformat [-train,"kWh"]
plot(yhat,sf.test)
abline (0,1)
mean((yhat -sf.test)^2)

yhat=predict (prune.sf, newdata =sampleformat [-train,])
sf.test=sampleformat [-train,"kWh"]
plot(yhat,sf.test)
abline (0,1)
mean((yhat -sf.test)^2)


regtree=accuracy(yhat,sf.test)
regtree
write.csv(regtree,"Performance Metrics Regression Tree.csv",row.names = FALSE)


###### NEURAL NETWORK 2nd APPROACH ######

set.seed(500)
library(MASS)

sampleformat <- sampleformat[, -c(1,2)]
View(sampleformat)

sampleformat <- sampleformat[, -c(4)]


#Checking missing data point
apply(sampleformat,2,function(x) sum(is.na(x)))

View(sampleformat)

# Train-test random splitting for linear model
index <- sample(1:nrow(sampleformat),round(0.75*nrow(sampleformat)))
train <- sampleformat[index,]
test <- sampleformat[-index,]

# Fitting linear model


lm.fit <- glm(kWh~.-day -hour, data=train)
summary(lm.fit)

# Predicted data from lm

pr.lm <- predict(lm.fit,test)

# Test MSE
MSE.lm <- sum((pr.lm - test$kWh)^2)/nrow(test)

# Neural net fitting


# Scaling data for the NN

sampleformat$Weekday <- as.numeric(sampleformat$Weekday)


maxs <- apply(sampleformat, 2, max) 
mins <- apply(sampleformat, 2, min)
scaled <- as.data.frame(scale(sampleformat, center = mins, scale = maxs - mins))

# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]
View(train_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("kWh ~", paste(n[!n %in% "kWh"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5), threshold= 0.5, linear.output=F)
print(nn)
# Visual plot of the model
plot(nn)

summary(nn)

# Predict and the covariate value should be same as nn's covariate number
pr.nn <- compute(nn,test_[,1:7])


summary(test_)
# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(sampleformat$kWh)-min(sampleformat$kWh))+min(sampleformat$kWh)
test.r <- (test_$kWh)*(max(sampleformat$kWh)-min(sampleformat$kWh))+min(sampleformat$kWh)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

# Plot predictions
par(mfrow=c(1,2))

plot(test$kWh,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$kWh,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Compare predictions on the same plot
plot(test$kWh,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$kWh,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#-------------------------------------------------------------------------------
# Cross validating

library(boot)
set.seed(200)

# Linear model cross validation

lm.fit <- glm(kWh~.,data=sampleformat)
cv.glm(sampleformat,lm.fit,K=10)$delta[1]


# Neural net cross validation
set.seed(450)
cv.error <- NULL
k <- 10

# Initialize progress bar
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(sampleformat),round(0.9*nrow(sampleformat)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,5),threshold= 0.5, linear.output=F)
  
  
  pr.nn <- compute(nn,test.cv[,1:7])
  pr.nn <- pr.nn$net.result*(max(sampleformat$kWh)-min(sampleformat$kWh))+min(sampleformat$kWh)
  
  test.cv.r <- (test.cv$kWh)*(max(sampleformat$kWh)-min(sampleformat$kWh))+min(sampleformat$kWh)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

# Average MSE
mean(cv.error)

# MSE vector from CV
cv.error

# Visual plot of CV results
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

