setwd("E:/gradePaper/result")
data_clean<-read.table("4_1&1_2.txt")
colnames(data_clean)<-c("x1","x2","x3","x4","lable")
#data_clean$x1<-(data_clean$x1)*100
data_clean$lable<-factor(data_clean$lable)
#test_set<-read.table("data_clean_matrix.txt")
#test_set$x1<-(test_set$x1)*100
#colnames(test_set)<-c("x1","x2","x3","x4","lable")
#test_set$lable<-factor(test_set$lable)
#glm.dataframe<-data.frame(AUC=c(),ACC=c(),Sensi=c(),Speci=c(),Preci=c(),Recall=c())
#colnames(glm.dataframe)<-c("AUC","ACC","Sensi","Speci","Preci","Recall")
#nb.dataframe<-data.frame(AUC=c(),ACC=c(),Sensi=c(),Speci=c(),Preci=c(),Recall=c())
#colnames(nb.dataframe)<-c("AUC","ACC","Sensi","Speci","Preci","Recall")
library(e1071)
library(caret)
library(pROC)
library(randomForest)
library(klaR)
#data_pos<-data[which(data$lable=="1"),]
#data_nav<-data[which(data$lable=="0"),]
control<-trainControl(method = "repeatedcv",number = 10,repeats = 10,classProbs = T)
#for(i in 1:20){
set.seed(123)
#ind<-sample(nrow(data_nav),nrow(data_pos))
#data_nav<-data_nav[ind,]
#data_clean<-rbind(data_nav,data_pos)
levels(data_clean$lable) <- list(no="0", yes="1")
levels(test_set$lable) <- list(no="0", yes="1")
index<-sample(2,nrow(data_clean),replace = T,prob=c(0.9,0.1))
train_set<-data_clean[index==1,]
test_set<-data_clean[index==2,]

glm.model<-train(lable~.,data = train_set,method="glm",metric="ROC",trControl=control)
nb.model<-train(lable~.,data = train_set,method="nb",metric="ROC",trControl=control)
svm.model<-train(lable~.,data = train_set,method="svmRadial",metric="ROC",trControl=control)

glm_pred<-predict(glm.model,test_set,type = "prob")
nb_pred<-predict(nb.model,test_set,type="prob")
svm_pred<-predict(svm.model,test_set,type="prob")

svm_pred_con<-predict(svm.model,test_set)
glm_pred_con<-predict(glm.model,test_set)
nb_pred_con<-predict(nb.model,test_set)

glm.roc<-roc(response=test_set$lable,predictor=glm_pred$yes,levels=levels(test_set$lable))
plot(glm.roc,type="S",col="green")
svm.roc<-roc(response=test_set$lable,predictor=svm_pred$yes,levels=levels(test_set$lable))
plot(svm.roc,add=T,col="blue")
nb.roc<-roc(response=test_set$lable,predictor=nb_pred$yes,levels=levels(test_set$lable))
plot(nb.roc,add=T,col="red")

confusionMatrix(glm_pred_con,test_set$lable,positive = "yes")
confusionMatrix(nb_pred_con,test_set$lable,positive = "yes")
confusionMatrix(svm_pred_con,test_set$lable,positive = "yes")
#glm.dataframe[i,1]=glm.roc$auc
#glm.dataframe[i,2]=glm.con$overall["Accuracy"]
#glm.dataframe[i,3]=glm.con$byClass["Sensitivity"]
#glm.dataframe[i,4]=glm.con$byClass["Specificity"]
#glm.dataframe[i,5]=glm.con$byClass["Precision"]
#glm.dataframe[i,6]=glm.con$byClass["Recall"]

#nb.dataframe[i,1]=nb.roc$auc
#nb.dataframe[i,2]=nb.con$overall["Accuracy"]
#nb.dataframe[i,3]=nb.con$byClass["Sensitivity"]
#nb.dataframe[i,4]=nb.con$byClass["Specificity"]
#nb.dataframe[i,5]=nb.con$byClass["Precision"]
#nb.dataframe[i,6]=nb.con$byClass["Recall"]
#}
