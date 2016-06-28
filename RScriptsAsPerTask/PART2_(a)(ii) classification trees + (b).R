library (ISLR)
attach (Hourly_filled_data)
#Transform and compute KWH_Class variable 
KWH_Class = ifelse ( kWh >mean(Hourly_filled_data$kWh) , "Above_Normal", "Optimal")
Hourly_filled_data = data.frame(Hourly_filled_data,KWH_Class)
summary(Hourly_filled_data)
str(Hourly_filled_data)
Hourly_filled_data$kWh= as.numeric(Hourly_filled_data$kWh)
#Hourly_filled_data$KWH_Class= as.numeric(Hourly_filled_data$KWH_Class)

#Use  variables  to fit a classification tree
library(tree)
tree = tree(KWH_Class ~ .-Account  -year -kWh  ,Hourly_filled_data)
summary(tree)

#Display the tree structure and node labels
plot(tree)
text(tree, pretty =0) #Pretty=0 includes the category names

#Split the dataset into a training set and a test set
set.seed (2)
train = sample (1:nrow(Hourly_filled_data), nrow(Hourly_filled_data)/2)

Hourly_filled_data.test = Hourly_filled_data [-train,]
KWH_Class.test = KWH_Class[-train]
#Build the tree based on the training set
tree.train = tree(KWH_Class ~ . -Account -year -kWh  , Hourly_filled_data, subset = train)
#Evaluate its performance on the test data
tree.pred = predict(tree.train, Hourly_filled_data.test, type = "class")
table(tree.pred, KWH_Class.test)
summary(tree.train)

#Determine the optimal level
set.seed (3)
#FUN = prune.misclass indicate that classification error rate is used to 
#guide the cross-validation and pruning process
cv.Hourly_filled_data = cv.tree(tree, FUN = prune.misclass)
names(cv.Hourly_filled_data)
cv.Hourly_filled_data

#Prune the tree
prune.Hourly_filled_data = prune.misclass(tree, best =9)
plot(prune.Hourly_filled_data)
text(prune.Hourly_filled_data, pretty =0)

#The pruned tree performance
prune.pred = predict(prune.Hourly_filled_data, Hourly_filled_data.test,type = "class")
table(prune.pred, KWH_Class.test)
summary(prune.Hourly_filled_data)

#Export dataset
#data(Hourly_filled_data, package="ISLR")
#write.csv(Hourly_filled_data,"Hourly_filled_data with KWH_Class.csv",row.names = FALSE)

#Building another tree model which will be useful while forecasting

attach (Hourly_filled_data2)
#Transform and compute KWH_Class variable 
KWH_Class2 = ifelse ( kWh >mean(Hourly_filled_data2$kWh) , "Above_Normal", "Optimal")
Hourly_filled_data2 = data.frame(Hourly_filled_data2,KWH_Class2)
summary(Hourly_filled_data2)
str(Hourly_filled_data2)
Hourly_filled_data2$kWh= as.numeric(Hourly_filled_data2$kWh)
Hourly_filled_data2$KWH_Class2= as.numeric(Hourly_filled_data2$KWH_Class2)

#Use  variables  to fit a classification tree
library(tree)
tree2 = tree(KWH_Class2 ~ .-Account  -year -kWh  ,Hourly_filled_data2)
summary(tree2)

#Display the tree structure and node labels
plot(tree2)
text(tree2, pretty =0) #Pretty=0 includes the category names

#Split the dataset into a training set and a test set
set.seed (2)
train2 = sample (1:nrow(Hourly_filled_data2), nrow(Hourly_filled_data2)/2)

Hourly_filled_data.test2 = Hourly_filled_data2 [-train2,]
KWH_Class.test2 = KWH_Class2[-train2]


tree.train2 = tree(KWH_Class2 ~ . -Account -year -kWh  , Hourly_filled_data2, subset = train2)