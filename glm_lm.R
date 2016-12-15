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
