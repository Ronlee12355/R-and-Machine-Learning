setwd("E:/deep_learing_R")
library(e1071)
data<-read.csv(file = "letterdata.csv",stringsAsFactors = F)
#data$letter<-as.numeric(data$letter)
data$letter<-as.factor(data$letter)#remember to make the lable factor
data_train<-data[1:16000,]
data_test<-data[16001:20000,]
svm.model<-svm(letter~.,data = data_train)
data_pred<-predict(svm.model,data_test)
result<-data_pred==data_test$letter
prop.table(table(result))
