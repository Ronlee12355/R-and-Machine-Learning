setwd("E:/quanyuan_my/NG/53")
data_clean<-read.table("output7_0504_1.txt",sep = "\t")
#raw_data_all<-read.table("4-paras-53.txt",sep = "\t")
colnames(data_clean)<-c("x1","x2","x3","x4","lable")
#raw_data<-subset(raw_data_all,select = c(V3,V4,V7,V8,V9))
#colnames(raw_data)<-c("x1","x2","x3","x4","lable")
data_clean$lable<-factor(data_clean$lable)

select_2_func<-function(x,y,z){
  p<-c()
  for(i in 1:length(x)){
    tmp<-c(x[i],y[i],z[i])
    if(max(tmp)<0.5 | min(tmp)>0.5){
      p[i]=mean(tmp)
    }else{
      if(length(tmp[which(tmp<0.5)])==1){
        p[i]=mean(tmp[which(tmp>0.5)])
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
#1：1将数据分类
data_pos<-data_clean[data_clean$lable==1,]
data_nav<-data_clean[data_clean$lable==0,]
index<-sample(nrow(data_nav),nrow(data_pos))
data_clean_1<-rbind(data_nav[index,],data_pos)
data_clean_1<-data_clean_1[order(runif(nrow(data_clean_1))),]
auc1<-c()
for(i in 1:1000){
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
  return(select_2_func(glm_pred1,nb_pred1,svm_pred1))
})
prob_3models_all<-c(prob_3models$Fold1,prob_3models$Fold2,prob_3models$Fold3,prob_3models$Fold4,prob_3models$Fold5)
data_clean_1_fold<-rbind(data_clean_1[folds$Fold1,],data_clean_1[folds$Fold2,],data_clean_1[folds$Fold3,],data_clean_1[folds$Fold4,],data_clean_1[folds$Fold5,])
preed1=prediction(prob_3models_all,data_clean_1_fold$lable)
pred.auc1<-performance(preed1,"auc")
auc1[i]<-unlist(pred.auc@y.values)
}
#1：3将数据分类
index3<-sample(nrow(data_nav),nrow(data_pos)*3)
data_clean_3<-rbind(data_nav[index,],data_pos)
data_clean_3<-data_clean_3[order(runif(nrow(data_clean_3))),]
auc3<-c()
for(i in 1:100){
  folds3<-createFolds(data_clean_3$lable,k=5)
  prob_3models_3<-lapply(folds3, function(x){
    train_3<-data_clean_3[-x,]
    test_3<-data_clean_3[x,]
    svm3<-svm(lable~.,data = train_3,probability=T,gamma=1,cost=25)
    glm3<-glm(lable~.,data = train_3,family=binomial)
    nb3<-naiveBayes(lable~.,data = train_3)
    glm_pred3<-predict(glm3,test_3,type="response")
    nb_pred3<-c(predict(nb3,test_3,type = "raw")[,2])
    svm_pred3<-predict(svm3,test_3,probability = T)
    svm_pred3<-c(attr(svm_pred3,"probabilities")[,2])
    return(select_2_func(glm_pred3,nb_pred3,svm_pred3))
  })
  prob_3models_all3<-c(prob_3models_3$Fold1,prob_3models_3$Fold2,prob_3models_3$Fold3,prob_3models_3$Fold4,prob_3models_3$Fold5)
  preed3=prediction(prob_3models_all3,rbind(data_clean_3[folds3$Fold1,],data_clean_3[folds3$Fold2,],data_clean_3[folds3$Fold3,],data_clean_3[folds3$Fold4,],data_clean_3[folds3$Fold5,])$lable)
  pred.auc3<-performance(preed3,"auc")
  auc3[i]<-unlist(pred.auc3@y.values)
}

index5<-sample(nrow(data_nav),nrow(data_pos)*5)
data_clean_5<-rbind(data_nav[index,],data_pos)
data_clean_5<-data_clean_5[order(runif(nrow(data_clean_5))),]
auc5<-c()
for(i in 1:100){
  folds5<-createFolds(data_clean_5$lable,k=5)
  prob_3models_5<-lapply(folds5, function(x){
    train_5<-data_clean_5[-x,]
    test_5<-data_clean_5[x,]
    svm5<-svm(lable~.,data = train_5,probability=T,gamma=1,cost=25)
    glm5<-glm(lable~.,data = train_5,family=binomial)
    nb5<-naiveBayes(lable~.,data = train_5)
    glm_pred5<-predict(glm5,test_5,type="response")
    nb_pred5<-c(predict(nb5,test_5,type = "raw")[,2])
    svm_pred5<-predict(svm5,test_5,probability = T)
    svm_pred5<-c(attr(svm_pred5,"probabilities")[,2])
    return(select_2_func(glm_pred5,nb_pred5,svm_pred5))
  })
  prob_3models_all5<-c(prob_3models_5$Fold1,prob_3models_5$Fold2,prob_3models_5$Fold3,prob_3models_5$Fold4,prob_3models_5$Fold5)
  preed5=prediction(prob_3models_all5,rbind(data_clean_5[folds5$Fold1,],data_clean_5[folds5$Fold2,],data_clean_5[folds5$Fold3,],data_clean_5[folds5$Fold4,],data_clean_5[folds5$Fold5,])$lable)
  pred.auc5<-performance(preed5,"auc")
  auc5[i]<-unlist(pred.auc5@y.values)
}
