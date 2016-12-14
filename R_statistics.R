sample(1:10,size = 5)#随机生成1:10的数字，size表示需要多大的范围
sample(c(0,1),10,replace = T)
data("mtcars")
range(mtcars$mpg)#检查数值范围
sd(mtcars$mpg)#标准差
var(mtcars$mpg)#方差
library(ggplot2)
library(reshape2)
qplot(x=Var1,y=Var2,data = melt(cor(mtcars[1:3])),fill=value,geom = "tile")

#二项分布检验
binom.test(x=92,n=315,p=1/6)

#t检验
boxplot(mtcars$mpg,mtcars$mpg[mtcars$am==0],ylab="mpg",names = c("overall","auto"))
abline(h=mean(mtcars$mpg),col="red",lwd=2)
abline(h=mean(mtcars$mpg[mtcars$am==0]),col="blue",lwd=2)
t.test(mtcars$mpg[mtcars$am==0],mu = mean(mtcars$mpg))
