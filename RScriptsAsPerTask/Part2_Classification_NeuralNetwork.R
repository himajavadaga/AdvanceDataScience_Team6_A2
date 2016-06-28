#x=file.choose(new = FALSE)

#Hourly_filled_data<-read.csv(x, header = TRUE)

attach(Hourly_filled_data3)

#View(Hourly_filled_data3)
str(Hourly_filled_data3)
#Hourly_filled_data3$Date <- as.character(Hourly_filled_data3$Date)
#Hourly_filled_data3$Date <- as.Date(Hourly_filled_data3$Date, format="%Y-%m-%d")
#Mean_KWH <- mean(Hourly_filled_data3$kWh)
#Hourly_filled_data3$KWH_Class[Hourly_filled_data3$kWh > Mean_KWH] <- 1
#Hourly_filled_data3$KWH_Class[Hourly_filled_data3$kWh <= Mean_KWH] <- 0

#Transform and compute KWH_Class variable 
Hourly_filled_data3$kWh= as.numeric(Hourly_filled_data3$kWh)
KWH_Class = ifelse ( kWh >mean(Hourly_filled_data3$kWh) , "Above_Normal", "Optimal")
Hourly_filled_data3 = data.frame(Hourly_filled_data3,KWH_Class)
summary(Hourly_filled_data3)
str(Hourly_filled_data3)
#Hourly_filled_data3$kWh= as.numeric(Hourly_filled_data3$kWh)

library(neuralnet)

Hourly_filled_data3$Weekday= as.character(Hourly_filled_data3$Weekday)
Hourly_filled_data3$Weekday= as.numeric(Hourly_filled_data3$Weekday)
Hourly_filled_data3$KWH_Class= as.numeric(Hourly_filled_data3$KWH_Class)

neuralnet <- neuralnet(KWH_Class ~ month + day + hour + DayofWeek + Weekday + Peakhour + Temperature ,data = Hourly_filled_data3, hidden = 4, err.fct = "sse", linear.output = FALSE)

neuralnet$result.matrix

plot(neuralnet)

###################Confusion Matrix#####################

set.seed(500)
library(MASS)

Hourly_filled_data_3_removed <- Hourly_filled_data3 

# Train-test random splitting for linear model
neuindex <- sample(1:nrow(Hourly_filled_data_3_removed),round(0.75*nrow(Hourly_filled_data_3_removed)))
neutrain <- Hourly_filled_data_3_removed[neuindex,]
neutest <- Hourly_filled_data_3_removed[-neuindex,]

#Normalize the value to be predicted
neuideal <- nnet::class.ind(Hourly_filled_data_3_removed$KWH_Class)

#Train the model, -11 because you want to leave out the KWH_Class attribute , the dataset had a total of 11 attributes with the last one as the predicted one

neuANN = nnet::nnet(Hourly_filled_data_3_removed[neutrain,-11], ideal[neutrain,], size=10, softmax=TRUE)


neuralnettrain <- neuralnet(KWH_Class ~ month + day + hour + DayofWeek + Weekday + Peakhour + Temperature ,data = neutrain, hidden = 4, err.fct = "sse", linear.output = FALSE)

predneural <- predict(neuralnettrain,neutest)
 
#maxs <- apply(Hourly_filled_data_3_removed, 2, max) 
#mins <- apply(Hourly_filled_data_3_removed, 2, min)
#scaled <- as.data.frame(scale(Hourly_filled_data_3_removed, center = mins, scale = maxs - mins))