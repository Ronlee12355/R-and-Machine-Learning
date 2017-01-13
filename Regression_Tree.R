setwd("E:/deep_learing_R")
wine<-read.csv(file = "whitewines.csv")
hist(wine$quality)#检查是否具有极端值
wine_train<-wine[1:3750,]
wine_test<-wine[3751:4898,]
library(rpart)
m_wine<-rpart(quality~.,data = wine_train)
library(rpart.plot)
rpart.plot(m_wine,digits=3)
p_wine<-predict(m_wine,wine_test)
cor(p_wine,wine_test$quality)#预测值和真实值的相关性

MAE<-mean(abs(p_wine - wine_test$quality))#平均绝对误差，查看预测值离真实值有多远

data("diamonds")
str(diamonds)
index<-sample(2,ncol(diamonds),replace = T,prob = c(0.9,0.1))
trainDia<-diamonds[index==1,]
testDia<-diamonds[index==2,]
#观察数据的总结情况
summary(trainDia)
symnum(cor(trainDia[,-c(2,3,4)]))

#使用图形来观察数据情况
library(lattice)
bwplot(depth~color,data=trainDia)

lm_model <- lm(price~.,data=trainDia)
par(mfrow=c(2,2))
plot(lm_model)

library(caret)
cr<-trainControl(method = "repeatedcv",number = 10,repeats = 10)
rpart_model<-train(price~.,data = trainDia,method = "rpart",trControl =cr,preProcess="scale")

library(rpart)
model<-rpart(price~.,data = trainDia,cp=0.03359724)
model_modified<-prune(model,cp=0.2)
