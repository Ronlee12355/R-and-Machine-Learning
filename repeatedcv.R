setwd("E:/quanyuan_my/NG/53")
data_clean<-read.table("output7_0504_1.txt",sep = "\t")
#raw_data_all<-read.table("4-paras-53.txt",sep = "\t")
colnames(data_clean)<-c("x1","x2","x3","x4","lable")
#raw_data<-subset(raw_data_all,select = c(V3,V4,V7,V8,V9))
#colnames(raw_data)<-c("x1","x2","x3","x4","lable")
data_clean$lable<-factor(data_clean$lable)

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
#引入数据包
library(e1071)
library(caret)
library(pROC)
library(klaR)
library(ROCR)
library(gplots)

data_pos<-data_clean[data_clean$lable==1,]
data_nav<-data_clean[data_clean$lable==0,]
index<-sample(nrow(data_nav),nrow(data_pos))
data_clean_1<-rbind(data_nav[index,],data_pos)
data_clean_1<-data_clean_1[order(runif(nrow(data_clean_1))),]

set.seed(123)
folds<-createFolds(data_clean_1$lable,k=5)
prob_3models<-lapply(folds, function(x){
  train_1<-data_clean_1[-x,]
  test_1<-data_clean_1[x,]
  svm1<-svm(lable~.,data = train_1,probability=T,gamma=1,cost=25)
  glm1<-glm(lable~.,data = train_1,family=binomial)
  nb1<-naiveBayes(lable~.,data = train_1)
  glm_pred1<-predict(glm1,test_1,type="response")
  nb_pred1<-c(predict(nb1,test_1,type = "raw")[,2])
  svm_pred1<-predict(svm1,test_1,probability = T)
  svm_pred1<-c(attr(svm_pred1,"probabilities")[,2])
  return(select_3_func(glm_pred1,nb_pred1,svm_pred1))
})

prob_3models_all<-c(prob_3models$Fold1,prob_3models$Fold2,prob_3models$Fold3,prob_3models$Fold4,prob_3models$Fold5)
preed1=prediction(prob_3models_all,rbind(data_clean_1[folds$Fold1,],data_clean_1[folds$Fold2,],data_clean_1[folds$Fold3,],data_clean_1[folds$Fold4,],data_clean_1[folds$Fold5,])$lable)
perf1=performance(preed1,"tpr","fpr")
plot(perf1,type="S",col=rgb(r=192/255,g=0,b=0),lwd=4)
