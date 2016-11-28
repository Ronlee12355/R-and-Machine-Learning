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
