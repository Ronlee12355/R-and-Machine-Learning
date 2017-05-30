library(DMwR)
str(sales)
library(Hmisc)
describe(sales)
barplot(table(sales$Insp))#不同欺诈结果的数量
barplot(table(sales$ID))#不同销售人员的数据
sales$Uprice<-sales$Val/sales$Quant#没一件物品的单价
summary(sales$Uprice)
head(sales$Uprice)
upp<-aggregate(sales$Uprice,list(sales$Prod),median,na.rm=T)
library(plyr)
upp<-arrange(upp,-x)
topP=data.frame(expensive=c("p3689","p2453","p2452","p2456","p2459"),"cheap"=c("p560","p559","p4195","p601","p563"))
tops<-sales[sales$Prod %in% topP[1,1],c("Prod","Uprice")]
tops<-rbind(tops,sales[sales$Prod %in% topP[1,2],c("Prod","Uprice")])
tops$Prod<-as.factor(tops$Prod)
library(ggplot2)
ggplot(tops,aes(x=Prod,y=log10(Uprice)))+geom_boxplot()

best_people<-aggregate(sales$Val,list(sales$ID),sum,na.rm=T)
best_people_scores<-sapply(c(T,F), function(i){
  return(best_people[order(best_people$x,decreasing = i)[1:5],1])
})
colnames(best_people_scores)<-c("most","least")

