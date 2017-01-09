library(C50)
library(caret)
library(pROC)
data(churn)
index<-sample(2,nrow(churnTrain),replace = T,prob = c(0.9,0.1))
trainset<-churnTrain[index==1,]
testset<-churnTrain[index==2,]
#进行10次重复10折cv
control<-trainControl(method = "repeatedcv",number = 10,repeats = 10,classProbs = T)
#构建模型
glm.model<-train(churn~.,data = trainset,method="glm",metric="ROC",trControl=control)
svm.model<-train(churn~.,data = trainset,method="svmRadial",metric="ROC",trControl=control)
rpart.model<-train(churn~.,data = trainset,method="rpart",metric="ROC",trControl=control)

#预测结果
glm.pred<-predict(glm.model,testset,type="prob")
svm.pred<-predict(svm.model,testset,type="prob")
rpart.pred<-predict(rpart.model,testset,type="prob")

#画出roc曲线
glm.roc<-roc(response=testset$churn,predictor=glm.pred$yes,levels=levels(testset$churn))
plot(glm.roc,type="S",col="green")
svm.roc<-roc(response=testset$churn,predictor=svm.pred$yes,levels=levels(testset$churn))
plot(svm.roc,add=T,col="blue")
rpart.roc<-roc(response=testset$churn,predictor=rpart.pred$yes,levels=levels(testset$churn))
plot(rpart.roc,add=T,col="red")
