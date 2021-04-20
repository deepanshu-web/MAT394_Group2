library(readxl)
df <- read_excel("E Commerce Dataset.xlsx")
head(df)


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


#changing repeated variable names
df$PreferredLoginDevice<-replace(df$PreferredLoginDevice,df$PreferredLoginDevice=="Phone","Mobile Phone")
df$PreferredPaymentMode<-replace(df$PreferredPaymentMode,df$PreferredPaymentMode=="Cash on Delivery","COD")



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
train<-train[complete.cases(train),]
test<-test[complete.cases(test),]
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

rf_random<-randomForest(Churn~.,data=train,importance=TRUE)

print(rf_random)
pred_rf<-predict(rf_random,test,type="class")

#test accuracy
mean(pred_rf==test$Churn)
#accuracy=96.8%


