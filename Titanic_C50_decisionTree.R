setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
titanic<-read.csv(file = "titanic.csv",na.strings = c("NA",""))
str(titanic)
titanic$Survived<-as.factor(titanic$Survived)
titanic$Pclass<-as.factor(titanic$Pclass)
library(Amelia)
missmap(titanic,main = "missing map")#画出缺失值图
#处理embarked的缺失值
table(titanic$Embarked,useNA = "ifany")
titanic$Embarked[which(is.na(titanic$Embarked))]='S'

#处理age的缺失值
library(stringr)
tb<-cbind(titanic$Age,str_match(titanic$Name,"[a-zA-Z]+\\."))
table(tb[is.na(tb[,1]),2])
mean_mr<-mean(titanic$Age[grepl("Mr\\.",titanic$Name) & !is.na(titanic$Age)])
mean_ms<-mean(titanic$Age[grepl("Mrs\\.",titanic$Name) & !is.na(titanic$Age)])
mean_dr<-mean(titanic$Age[grepl("Dr\\.",titanic$Name) & !is.na(titanic$Age)])
mean_miss<-mean(titanic$Age[grepl("Miss\\.",titanic$Name) & !is.na(titanic$Age)])
mean_master<-mean(titanic$Age[grepl("Master\\.",titanic$Name) & !is.na(titanic$Age)])

titanic$Age[grepl("Mr\\.",titanic$Name) & is.na(titanic$Age)]=mean_mr
titanic$Age[grepl("Mrs\\.",titanic$Name) & is.na(titanic$Age)]=mean_ms
titanic$Age[grepl("Dr\\.",titanic$Name) & is.na(titanic$Age)]=mean_dr
titanic$Age[grepl("Miss\\.",titanic$Name) & is.na(titanic$Age)]=mean_miss
titanic$Age[grepl("Master\\.",titanic$Name) & is.na(titanic$Age)]=mean_master

#进行数据可视化
barplot(table(titanic$Survived),main = "存活情况",names.arg = c("perrished","survived"))
#注意table前后不同数据，带来的结果也不一样
barplot(table(titanic$Survived,titanic$Sex),col = c("blue","red"),main = "不同性别存活情况",legend.text = c("死亡","存活"))

#开始使用决策树进行预测
titanic_train<-titanic[1:802,]
titanic_test<-titanic[803:891,]
library(C50)
titanic_model<-C5.0(Survived~Pclass+Sex+Age+SibSp+Fare+Parch+Embarked,data = titanic_train)
plot(titanic_model)
library(caret)
titanic_pred<-predict(titanic_model,titanic_test)
confusionMatrix(titanic_pred,titanic_test$Survived)

#画出roc曲线
library(ROCR)
titanic_pred_prob<-predict(titanic_model,titanic_test,type = "prob")[,1]
pred<-prediction(predictions = titanic_pred_prob,labels = titanic_test$Survived)
pref<-performance(pred,measure = "tpr",x.measure = "fpr")
plot(pref,main="ROC curve",col="blue",lwd=3)
auc<-performance(pred,measure = "auc")
str(auc)
unlist(auc@y.values)#计算出auc曲线面积
