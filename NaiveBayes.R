setwd("E:/deep_learing_R")
library(e1071)
library(caret)
library(MASS)
data("iris")
train<-iris[1:135,]
test<-iris[136:150,]
model<-naiveBayes(Species~.,data = train)
res<-predict(model,test[,1:4],type = "class")
summary(res)
confusionMatrix(table(res,test$Species))
