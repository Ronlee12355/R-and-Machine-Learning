setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
insurance<-read.csv(file = "insurance.csv",stringsAsFactors = T)
summary(insurance$charges)
hist(insurance$charges)#主要集中在0~15000间
cor(insurance[c("age","bmi","children","charges")])
ins_model<-lm(charges~.,data = insurance)
summary(ins_model)

#to improve the model
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)
ins_modeli<-lm(charges~age+I(age^2)+children+bmi+I(bmi^2)+sex+bmi30*smoker+region,data = insurance)
summary(ins_modeli)
