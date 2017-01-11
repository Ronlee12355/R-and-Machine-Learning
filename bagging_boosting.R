library(C50)
library(caret)
library(pROC)
library(adabag)
data(churn)
index<-sample(2,nrow(churnTrain),replace = T,prob = c(0.9,0.1))
trainset<-churnTrain[index==1,]
testset<-churnTrain[index==2,]

#构建模型并预测
set.seed(2)
churn.bagging<-bagging(churn~.,data = trainset,mfinal = 10)
pred<-predict(churn.bagging,churnTest)
pred$confusion

#bagging进行k折cv验证
churn.baggingcv<-bagging.cv(churn~.,data = trainset,v=10,mfinal = 100)
