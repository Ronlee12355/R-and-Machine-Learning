setwd("E:/deep_learing_R")
install.packages("C50")
library(C50)
credit<-read.csv(file = "credit.csv",stringsAsFactors = T)
set.seed(12345)
credit_random<-credit[order(runif(1000)),]
credit_random_train<-credit_random[1:900,]
credit_random_test<-credit_random[901:1000,]
prop.table(table(credit$default))
#type1
credit_model<-C5.0(credit_random_train[,-17],as.factor(credit_random_train[,17]))
credit_pred<-predict(credit_model,credit_random_test)
library(gmodels)
CrossTable(credit_random_test$default,credit_pred,prop.r = F,prop.c = F,prop.chisq = F,dnn = c("actual","predicted"))
#75%的准确率，只预测出65.6%的违约者

#type2
credit_model_boot10<-credit_model<-C5.0(credit_random_train[,-17],as.factor(credit_random_train[,17]),trials = 10)
credit_pred_boot10<-predict(credit_model_boot10,credit_random_test)
CrossTable(credit_random_test$default,credit_pred_boot10,prop.r = F,prop.c = F,prop.chisq = F,dnn = c("actual","predicted"))
#78%的准确率，只预测出50%的违约者
