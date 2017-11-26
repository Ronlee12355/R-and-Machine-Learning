library(gcookbook)
str(cabbage_exp)
library(ggplot2)
ggplot(data=cabbage_exp,mapping = aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat = "identity")
ggplot(data = diamonds,mapping = aes(x=cut))+geom_bar()
upc<-subset(uspopchange,rank(Change)>40)
ggplot(data = uspopchange,mapping = aes(x=Abb,y=Change,fill=Region))+
geom_bar(stat = "identity",color="red")

library(wordcloud2)
wordcloud2(demoFreqC,figPath = img,size = 1,color = "random-light")
wordcloud2(siwen,size = 1,color = "random-light",shape = "'cardioid")

ggplot(data = cabbage_exp,mapping = aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = "identity",width = 0.6,position = position_dodge(0.7))

ggplot(data = cabbage_exp,mapping = aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = "identity")+guides(fill=guide_legend(reverse = T))

ggplot(data = BOD,mapping = aes(x=Time,y=demand))+geom_line()
ggplot(tg,aes(x=dose,y=length,color=supp))+geom_line(linetype="dashed")+geom_point(size=2,shape=21)

house<-read.table("C:/Users/sdj/Desktop/housing.data",header = F)
library(stringr)
corrplot(cor(house),order = "AOE",addCoef.col = "grey",method = "color")
corrplot(cor(house),order = "AOE",addCoef.col = "grey")
house_clean<-subset(house,select = -c(V7,V3))
model<-glm(V14~.,data = house_clean[1:405,])
summary(model)
ret<-predict(model,house_clean[406:506,])

library(rpart)
rpart.model<-rpart(V14~.,data = house[1:405,])
library(rpart.plot)
rpart.plot(rpart.model,digits = 5)
rpart.predict<-predict(rpart.model,house[406:506,])
cor(rpart.predict,house[406:506,]$V14)
MAE<-mean(abs(rpart.predict-house[406:506,]$V14))

ggplot(data = cabbage_exp,mapping = aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = "identity",width = 0.6,position = position_dodge(0.7))

ggplot(data = cabbage_exp,mapping = aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = "identity")+guides(fill=guide_legend(reverse = T))

lotting
library(ggplot2)
library(gcookbook)
hg<-heightweight[,c("sex","ageYear","heightIn","weightLb")]
ggplot(data = hg,aes(x=ageYear,y=heightIn,size=weightLb,color=sex))+geom_point(alpha=0.7)+
  scale_size_area()+scale_color_brewer(palette = "Set1")

#线性回归
ggplot(data = heightweight,aes(x=ageYear,y=heightIn))+geom_point()+stat_smooth(method = "lm",level = 0.95)

#draw the picture of butterfly
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))
suppressMessages(library("showtext"))
mydata<-data.frame(id=1:14,A=c(5.0,14.7,2.5,8.5,5.1,6.9,7.7,6.8,4.4,4.9,5.3,1.0,0.9,7.8),
                   B=c(31.3,24.7,17.8,17.2,15.3,14.3,13.9,13.9,12.4,10.0,6.5,4.2,2.5,0.9),
                   Label=c("Website","Customer & Employee Referral","Webinar","Facebook/Twitter/Other Social","Marketting & Advertising","Paid Serch","Other","Sales generated","Tradeshows","Parter","Linkedin","Events","Lead list","Emial Campaign"))
p1<-ggplot(mydata,aes(x=id,y=A))+geom_hline(yintercept=mean(mydata$A),linetype=2,size=.25,colour="blue")+geom_bar(stat = "identity",fill="red")+
  geom_text(aes(x=id,y=-3.3,label=Label),vjust=0.5)+coord_flip()+ylim(-5.5,16)+geom_text(aes(y=A+0.75,label=paste(A,'%')),size=4)+
  theme_void()
p2<-ggplot(mydata,aes(x=id,y=-B))+geom_hline(yintercept=-mean(mydata$B),linetype=2,size=.25,colour="grey")+geom_bar(fill="#C44E4C",stat = "identity")+
  ylim(-40,0)+geom_text(aes(x=id,y=-B-1.8,label=paste(B,'%')),vjust=0.5)+coord_flip()+theme_void()

png("F:/butterfly.png",width = 1300,height =800)
showtext_begin()
grid.newpage()
pushViewport(viewport(layout = grid.layout(7,11)))
print(p2,vp=viewport(layout.pos.row = 2:7,layout.pos.col = 1:5))
print(p1,vp=viewport(layout.pos.row = 2:7,layout.pos.col = 6:11))
showtext_end()
dev.off()
