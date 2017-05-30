library(DMwR)
str(sales)
library(Hmisc)
describe(sales)
barplot(table(sales$Insp))#不同欺诈结果的数量
barplot(table(sales$ID))#不同销售人员的数据
sales$Uprice<-sales$Val/sales$Quant#没一件物品的单价
summary(sales$Uprice)
