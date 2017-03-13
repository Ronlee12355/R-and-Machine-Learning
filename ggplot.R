#2.4
hist(mtcars$mpg)
ggplot(mtcars,aes(x=mpg),col="red")+geom_histogram(binwidth = 3)#binwidth用于控制宽度
#2.5
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp+dose,ToothGrowth)
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()#interaction将两个变量组合
