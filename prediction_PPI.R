#预处理所有数据
setwd("/data4/jli/quanyuan_my/NG")
data_clean<-read.table("output7_1.txt")
raw_data<-read.table("8_1_p5_not_0.txt",sep = "\t")
colnames(data_clean)<-c("x1","x2","x3","x4","lable")
raw_data<-subset(raw_data,select = c(V5,V6,V7,V8,V9))
colnames(raw_data)<-c("x1","x2","x3","x4","lable")
data_clean$lable<-factor(data_clean$lable)

#引入数据包
library(e1071)
library(caret)
library(pROC)
library(klaR)
library(ROCR)
library(gplots)

#找出正负样本，便于取样
data_pos<-data_clean[which(data_clean$lable==1),]
data_nav<-data_clean[which(data_clean$lable==0),]
control<-trainControl(method = "repeatedcv",number = 10,repeats = 10,classProbs = T)

#对数据进行取样
set.seed(123)
ind<-sample(nrow(data_nav),nrow(data_pos)*5)
data_nav_1_5<-data_nav[ind,]
data_clean_1_5<-rbind(data_nav_1_5,data_pos)
levels(data_clean_1_5$lable) <- list(no="0", yes="1")
index<-sample(2,nrow(data_clean_1_5),replace = T,prob=c(0.9,0.1))
train_set<-data_clean_1_5[index==1,]
test_set<-data_clean_1_5[index==2,]

select_3_func<-function(x,y,z){
  p<-c()
  for(i in 1:length(x)){
    tmp<-c(x[i],y[i],z[i])
    if(max(tmp)<0.5 | min(tmp)>0.5){
      p[i]=mean(tmp)
    }else{
      if(length(tmp[which(tmp<0.5)])==1){
        p[i]=tmp[which(tmp<0.5)]
      }else{
        p[i]=mean(tmp[which(tmp<0.5)])
      }
    }
  }
  return(p)
}

#训练模型
glm.model5<-train(lable~.,data = train_set,method="glm",metric="ROC",trControl=control)
nb.model5<-train(lable~.,data = train_set,method="nb",metric="ROC",trControl=control)
svm.model5<-train(lable~.,data = train_set,method="svmRadial",metric="ROC",trControl=control)

svm_pred_con<-predict(svm.model5,test_set)
glm_pred_con<-predict(glm.model5,test_set)
nb_pred_con<-predict(nb.model5,test_set)

glm_pred<-predict(glm.model,test_set,type="prob")
nb_pred<-predict(nb.model,test_set,type="prob")
svm_pred<-predict(svm.model,test_set,type="prob")

glm_pred3<-predict(glm.model3,test_set,type="prob")
nb_pred3<-predict(nb.model3,test_set,type="prob")
svm_pred3<-predict(svm.model3,test_set,type="prob")

glm_pred5<-predict(glm.model5,test_set,type="prob")
nb_pred5<-predict(nb.model5,test_set,type="prob")
svm_pred5<-predict(svm.model5,test_set,type="prob")

#使用三选三的方式取值
select_3_1_result<-ifelse(glm_pred$yes>0.5 & nb_pred$yes>0.5 & svm_pred$yes>0.5,1,0)
select_3_3_result<-ifelse(glm_pred3$yes>0.5 & nb_pred3$yes>0.5 & svm_pred3$yes>0.5,1,0)
select_3_5_result<-ifelse(glm_pred5$yes>0.5 & nb_pred5$yes>0.5 & svm_pred5$yes>0.5,1,0)

#根据概率来取值
pred_1<-select_3_func(glm_pred$yes,nb_pred$yes,svm_pred$yes)
pred_3<-select_3_func(glm_pred3$yes,nb_pred3$yes,svm_pred3$yes)
pred_5<-select_3_func(glm_pred5$yes,nb_pred5$yes,svm_pred5$yes)

pred_1_roc<-roc(response=test_set$lable,predictor=pred_1,levels=levels(test_set$lable))
plot(pred_1_roc,type="S",col="black",lwd=4)
pred_3_roc<-roc(response=test_set$lable,predictor=pred_3,levels=levels(test_set$lable))
plot(pred_3_roc,add=T,col="blue",lwd=4)
pred_5_roc<-roc(response=test_set$lable,predictor=pred_5,levels=levels(test_set$lable))
plot(pred_5_roc,add=T,col="red",lwd=4)

#计算0,1的ROC曲线值
preed1=prediction(select_3_1_result,test_set$lable)
perf1=performance(preed1,"tpr","fpr")
preed3=prediction(select_3_3_result,test_set$lable)
perf3=performance(preed3,"tpr","fpr")
preed5=prediction(select_3_5_result,test_set$lable)
perf5=performance(preed5,"tpr","fpr")
plot(perf1,lwd=4,col="red")
plot(perf3,add=T,col="black",lwd=4)
plot(perf5,add=T,col="blue",lwd=4)

#画出ROC曲线
glm.roc<-roc(response=test_set$lable,predictor=glm_pred$yes,levels=levels(test_set$lable))
plot(glm.roc,type="S",col="black",lwd=4)
svm.roc<-roc(response=test_set$lable,predictor=svm_pred$yes,levels=levels(test_set$lable))
plot(svm.roc,add=T,col="blue",lwd=4)
nb.roc<-roc(response=test_set$lable,predictor=nb_pred$yes,levels=levels(test_set$lable))
plot(nb.roc,add=T,col="red",lwd=4)

#计算混淆举证的值
confusionMatrix(glm_pred_con,test_set$lable,positive = "yes")
confusionMatrix(nb_pred_con,test_set$lable,positive = "yes")
confusionMatrix(svm_pred_con,test_set$lable,positive = "yes")
