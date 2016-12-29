library(car)
data("Quartet")
plot(Quartet$x,Quartet$y1)
lmfit<-lm(y1~x,data = Quartet)
abline(lmfit,col="red")
newdata<-data.frame(x=c(3,6,15))
tes<-predict(lmfit,newdata,interval="predict")
par(mfrow=c(2,2))
plot(lmfit)

#多项式回归
plot(Quartet$x,Quartet$y2)
fit<-lm(Quartet$y2~Quartet$x+I(Quartet$x^2),data = Quartet)
summary(fit)

#使用rlm来生成稳健性回归模型，可以除掉孤立点的事情
library(MASS)
plot(Quartet$x,Quartet$y3)
fit1<-rlm(y3~x,data = Quartet)
abline(fit1,col="red")

#glm模型的使用
data("SLID")
lmfit_1<-glm(wages~age+sex+education,family = gaussian,data = SLID)
summary(lmfit_1)
lmfit_2<-lm(wages~age+sex+education,data = SLID)
anova(lmfit_1,lmfit_2)

library(C50)
data(churn)
fit<-glm(churn~.,data = trainset,family = binomial)
summary(fit)

fit2<-glm(churn~international_plan+voice_mail_plan+total_intl_calls
          +number_customer_service_calls,data = trainset,family = binomial)
summary(fit2)
pred<-predict(fit2,testset,type = "response")
pred<-ifelse(pred>0.5,0,1)
#pred<-as.factor(pred)
testset$churn<-ifelse(testset$churn=="yes",1,0)
library(caret)
confusionMatrix(table(testset$churn,pred))
