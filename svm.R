setwd("E:/deep_learing_R")
library(e1071)
data<-read.csv(file = "letterdata.csv",stringsAsFactors = F)
#data$letter<-as.numeric(data$letter)
data$letter<-as.factor(data$letter)#remember to make the lable factor
data_train<-data[1:16000,]
data_test<-data[16001:20000,]
svm.model<-svm(letter~.,data = data_train)
data_pred<-predict(svm.model,data_test)
result<-data_pred==data_test$letter
prop.table(table(result))

library(C50)
data(churn)
churnTrain<-churnTrain[,!names(churnTrain) %in% c("state","area_code","account_length")]
ind<-sample(2,nrow(churnTrain),replace = T,prob = c(0.9,0.1))#按比例分出数据集
trainset<-churnTrain[ind==1,]
testset<-churnTrain[ind==2,]
library(caret)
set.seed(300)
model<-train(churn~.,data=trainset,method="svmRadial")
library(e1071)
model_1071<-svm(churn~.,data=trainset,kernel="radial",cost=1)
pred_1071<-predict(model_1071,testset)
confusionMatrix(table(pred_1071,testset$churn),positive = "no")

con<-trainControl(method = "repeatedcv",number = 10,repeats = 10)
model_cv<-train(churn~.,data=trainset,trControl=con,method="svmRadial")
