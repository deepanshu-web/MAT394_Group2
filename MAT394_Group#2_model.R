library(readxl)
df <- read_excel("E Commerce Dataset.xlsx")
str(df)


library(dplyr)
library(ggplot2)
library(modeest)
library(Amelia)
library(caTools)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)



p<-ggplot(data=df,aes(x=Churn))
p+geom_histogram(binwidth=0.5, fill ="steelblue",color="red")


#Descriptive analysis 
dim(df)
summary(df)
str(df)


#converting categorical variables to factor variable

names<-c(2,4,5,7,8,11,12,13,15)
df[,names]<-lapply(df[,names],factor)



#checking for missing values
missmap(obj=df)

#replacing missing values with median

df$DaySinceLastOrder[is.na(df$DaySinceLastOrder)]<-median(df$DaySinceLastOrder,na.rm=TRUE)
df$OrderAmountHikeFromlastYear[is.na(df$OrderAmountHikeFromlastYear)]<-median(df$OrderAmountHikeFromlastYear,na.rm=TRUE)
df$Tenure[is.na(df$Tenure)]<-median(df$Tenure,na.rm=TRUE)
df$OrderCount[is.na(df$OrderCount)]<-median(df$OrderCount,na.rm=TRUE)
df$CouponUsed[is.na(df$CouponUsed)]<-median(df$CouponUsed,na.rm=TRUE)
df$HourSpendOnApp[is.na(df$HourSpendOnApp)]<-median(df$HourSpendOnApp,na.rm=TRUE)
df$WarehouseToHome[is.na(df$WarehouseToHome)]<-median(df$WarehouseToHome,na.rm=TRUE)


#creating the train and test dataset
set.seed(42)
split<-sample.split(df$CustomerID,SplitRatio =0.8 )
train<-subset(df,split=="TRUE")
test<-subset(df,split=="FALSE")
train$CustomerID<-NULL
test$CustomerID<-NULL


#logistic regression
logistic<- glm(Churn ~.,data=train,family=binomial)
summary(logistic)


probs<-predict(logistic,newdata=test, type="response")
pred<- ifelse(probs>0.5,1,0)
test$prediction<-pred

#checking accuracy
mean(test$Churn==test$prediction)


#accuraacy=89.7%

##random forest

rf_random<-randomForest(Churn~.,data=train,mtry=4,ntree=500, importance=TRUE)

print(rf_random)
pred_rf<-predict(rf_random,test,type="class")

#test accuracy
mean(pred_rf==test$Churn)
varImpPlot(rf_random)

#accuracy=96.8%


#K-fold cross validation with logistic regression
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10,search="grid")
# Train the model
model_logistic <- train(Churn ~., data = train , method = "glm",family="binomial",
               trControl = train.control)

# Summarize the results
print(model_logistic)
summary(model_logistic)

#accuracy check on test data
pred_logistic<-predict(model_logistic,test)
mean(pred_logistic==test$Churn)

#K-fold cross validation with random forest.

set.seed(30) 
train.control <- trainControl(method = "cv", number = 10,search="grid")
# Train the model
model_rf <- train( Churn ~ ., data = train, method = 'rf',
                    metric = 'Accuracy',
                    trControl = train.control)

# Summarize the results
print(model_rf)
summary(model_rf)


#accuracy check on test data
pred_rf<-predict(model_rf,test)
mean(pred_rf==test$Churn)





