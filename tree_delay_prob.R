setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
library(C50)
data(churn)
str(churnTrain)
churnTrain<-churnTrain[,!names(churnTrain) %in% c("state","area_code","account_length")]
set.seed(12345)
churnTrain<-churnTrain[order(runif(nrow(churnTrain))),]
trainset<-churnTrain[1:2666,]
testset<-churnTrain[2667:3333,]
library(rpart)
model<-rpart(churn~.,data = trainset)
summary(model)
plot(model,uniform = T)
text(model,all = T,use.n = T)
prediction<-predict(model,testset,type = "class")
table(prediction,testset$churn)
library(caret)
confusionMatrix(prediction,testset$churn)
