x=file.choose(new = FALSE)

Hourly_filled_data<-read.csv(x, header = TRUE)

attach (Hourly_filled_data1)

View(Hourly_filled_data1)
str(Hourly_filled_data1)

Hourly_filled_data1$Date <- as.character(Hourly_filled_data1$Date)

Hourly_filled_data1$Date <- as.Date(Hourly_filled_data1$Date, format="%Y-%m-%d")
#Hourly_filled_data1$month <- as.factor(Hourly_filled_data1$month)
#Hourly_filled_data1$day <- as.factor(Hourly_filled_data1$day)
#Hourly_filled_data1$year <- as.numeric(Hourly_filled_data1$year)
#Hourly_filled_data1$hour <- as.factor(Hourly_filled_data1$hour)
#Hourly_filled_data1$DayofWeek <- as.factor(Hourly_filled_data1$DayofWeek)
#Hourly_filled_data1$Weekday <- as.factor(Hourly_filled_data1$Weekday)
#Hourly_filled_data1$Peakhour <- as.factor(Hourly_filled_data1$Peakhour)
#Hourly_filled_data1$Temperature <- as.numeric(Hourly_filled_data1$Temperature)


Mean_KWH <- mean(Hourly_filled_data1$kWh)

Hourly_filled_data1$KWH_Class[Hourly_filled_data1$kWh > Mean_KWH] <- 1

Hourly_filled_data1$KWH_Class[Hourly_filled_data1$kWh <= Mean_KWH] <- 0

#Hourly_filled_data1$KWH_Class <- with(Hourly_filled_data1, ifelse(kWh>Mean_KWH, "Above_Normal", "Optimal"))

Hourly_filled_data1$KWH_Class <- factor(Hourly_filled_data1$KWH_Class,levels=c(0,1),labels=c("Optimal","Above_Normal"))

#Hourly_filled_data1$KWH_Class <- with(Hourly_filled_data1, ifelse(kWh>Mean_KWH, 1, 0))
smp_size <- floor(0.75 * nrow(Hourly_filled_data1))

set.seed(123)

logtrain_ind <- sample(seq_len(nrow(Hourly_filled_data1)), size = smp_size)

#Split the data into training and testing
logtrain <- Hourly_filled_data1[logtrain_ind, ]
logtest <- Hourly_filled_data1[-logtrain_ind, ]

logfit <- glm(KWH_Class ~. -Account -Date -kWh -day -year, data=logtrain, family=binomial(link="logit"))
summary(logfit)

#write.csv(logfit,"LogisticRegressionMetrics",row.names = FALSE)

#Run the model on the test set
logtest.probs <- predict(logfit, logtest, type='response')
logpred <- rep("Optimal",length(logtest.probs))

#Set the cutoff value =0.5
logpred[logtest.probs>=0.5] <- "Above_Normal"

#install.packages('e1071', dependencies=TRUE)
#Classification matrix
library(caret)
conmatrix <- confusionMatrix(logtest$KWH_Class, logpred)

#write.csv(conmatrix,"LogisticRegressionConfMatrix",row.names = FALSE)

#ROC curve
library(ROCR)
logprediction <- prediction(logtest.probs, logtest$KWH_Class)
logperformance <- performance(logprediction, measure = "tpr", x.measure = "fpr")
plot(logperformance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
logtest$probs=logtest.probs
logtest$prob=sort(logtest$probs,decreasing = T)
loglift <- lift(KWH_Class ~ prob, data = logtest)
loglift
xyplot(loglift,plot = "gain")
