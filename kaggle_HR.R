setwd("E:/kaggle/human resource")
library(data.table)
hr<-fread("HR_comma_sep.csv")
str(hr)
library(ggplot2)
library(plotly)
library(dplyr)
library(corrplot)
library(randomForest)
library(pROC)
hr$sales<-as.factor(hr$sales)
hr$salary<-as.factor(hr$salary)
ggplot(group_by(hr,sales),aes(x=sales,fill=sales))+geom_bar(width = 1)+coord_polar()+ggtitle("不同职业的人数")
ggplot(hr,aes(x=sales,y=satisfaction_level,fill=salary))+geom_boxplot()+ggtitle("不同职业的满意度")
ggplot(hr,aes(x=sales,y=average_montly_hours,fill=salary))+geom_boxplot()+ggtitle("不同职业的工作时长")

hr$left<-as.factor(hr$left)
ggplot(hr,aes(x=sales,y=number_project,fill=salary))+geom_boxplot()+ggtitle("不同职业的项目情况")

table(hr$Work_accident)
hr$Work_accident<-as.factor(hr$Work_accident)
ggplot(hr,aes(x=sales,y=Work_accident,fill=Work_accident))+geom_histogram(stat = "identity")+ggtitle("不同职业的工作失误情况")

ggplot(hr,aes(x=satisfaction_level,y=left,fill=left))+geom_density()+ggtitle("满意度和是否离职的关系")

p<-ggplot(hr,aes(x=satisfaction_level,color=left))+geom_line(stat = "density")+ggtitle("满意度和离职的关系")
ggplotly(p)

ggplotly(ggplot(hr,aes(x=average_montly_hours,color=left))+geom_line(stat = "density")+ggtitle("工作时长和离职的关系"))
ggplotly(ggplot(hr,aes(x=salary,fill=left))+geom_histogram(stat="count")+ggtitle("工资和离职的关系"))
ggplotly(ggplot(hr,aes(x=last_evaluation,color=left))+geom_point(stat = "count")+ggtitle("工资和离职的关系"))

cor.hr<-hr[,-c("sales","salary")]
cor.hr$Work_accident<-as.numeric(as.character(cor.hr$Work_accident))
cor.hr$left<-as.numeric(as.character(cor.hr$left))
corrplot(corr = cor(cor.hr),type = "lower",method = "number",title="变量相关性",order="AOE")

index<-sample(2,nrow(hr),replace = T,prob = c(0.7,0.3))
train<-hr[index==1,];test<-hr[index==2,]
model<-randomForest(left~.,data = train)
predict.hr<-predict(model,test)
confusionMatrix(test$left,predict.hr)

prob.hr<-predict(model,test,type="prob")
roc.hr<-roc(test$left,prob.hr[,2],levels=levels(test$left))
plot(roc.hr,type="S",col="red")
