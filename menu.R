setwd("E:/kaggle/nutrition-facts-for-mcdonald-s-menu")
suppressMessages(library(plotly))
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(gridExtra))
suppressMessages(library(RColorBrewer))
menu<-fread("menu.csv")
str(menu)
menu$Category<-as.factor(menu$Category)

#提取所有的饮品
menu_drink<-menu %>% filter(Category=="Beverages" | Category=="Coffee & Tea")

#观察不同category之间的热量高低
menu %>% group_by(Category) %>% summarise(heat=median(Calories)) %>%
  ggplot(aes(x=Category,y=heat))+geom_histogram(aes(fill=Category),stat = "identity")+
  labs(x="不同食物种类",y="热量中位数",subtitle="单位（j）")+ggtitle("不同种类食物的热量高低")

p1<-menu %>% ggplot(aes(x=Category,y=Calories,fill=Category))+geom_boxplot()+ggtitle("不同食物的热量分布")+
  ylim(c(0,1500))
p2<-menu %>% ggplot(aes(x=Category,y=Sugars,fill=Category))+geom_boxplot()+ggtitle("不同食物的糖分布")+
  ylim(c(0,150))
p3<-menu %>% ggplot(aes(x=Category,y=Protein,fill=Category))+geom_boxplot()+ggtitle("不同食物的蛋白质分布")+
  ylim(c(0,60))
p4<-menu %>% group_by(Category) %>% summarise(n=median(Sodium)) %>% ggplot(aes(x=Category,y=n,fill=Category))+
  geom_bar(stat = "identity")+coord_polar()+labs(y="Na元素的含量",title="不同种类物质的Na分布")
p5<-menu %>% ggplot(aes(x=Category,y=Carbohydrates,fill=Category))+geom_boxplot()+ggtitle("不同食物的碳水化合物分布")+
  ylim(c(0,150))
p6<-menu %>% ggplot(aes(x=Category,y=`Iron (% Daily Value)`,fill=Category))+geom_boxplot()+ggtitle("不同食物的铁元素分布")+
  ylim(c(0,50))
p7<-menu %>% ggplot(aes(x=Category,y=Cholesterol,fill=Category))+geom_boxplot()+ggtitle("不同食物的胆固醇分布")+
  ylim(c(0,150))
p8<-menu %>% ggplot(aes(x=Category,y=`Saturated Fat`,fill=Category))+geom_boxplot()+ggtitle("不同食物的胆固醇分布")+
  ylim(c(0,25))
grid.arrange(p1,p2,p3,p4,ncol=1)
grid.arrange(p5,p6,p7,p8,ncol=1)

#观察不同营养成分和热量的关系
pp1<-menu %>% ggplot(aes(x=`Total Fat`,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  geom_smooth(method = "lm",se=F)+scale_color_gradient(low="blue",high = "red")+ggtitle("脂肪和热量的分布关系")
pp2<-menu %>% ggplot(aes(x=Carbohydrates,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_size_area()+geom_smooth(method = "lm",se=F)+scale_color_distiller(palette = "Set1")+ggtitle("碳水化合物与热量的关系")
pp3<-menu %>% ggplot(aes(x=`Dietary Fiber`,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_size_area()+scale_color_distiller(palette = "Set2")+ggtitle("膳食纤维与热量的关系")
pp4<-menu %>% ggplot(aes(x=Sugars,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_size_area()+geom_smooth(method = "lm",se=F)+scale_color_distiller(palette = "Set3")+ggtitle("糖与热量的关系")
pp5<-menu %>% ggplot(aes(x=Protein,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_size_area()+geom_smooth(method = "lm",se=F,color="red")+scale_color_distiller()+ggtitle("蛋白质与热量的关系")
pp6<-menu %>% ggplot(aes(x=`Saturated Fat`,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_size_area()+geom_smooth(method = "lm",se=F)+scale_color_distiller(palette = "Dark2")+ggtitle("饱和脂肪与热量的关系")
grid.arrange(pp1,pp2,pp3,pp4,pp5,pp6,ncol=2)

menu %>% ggplot(aes(x=`Iron (% Daily Value)`,y=Calories,color=Calories))+geom_point(aes(size=Calories))+
  scale_color_distiller(palette = "Accent")+coord_polar()+scale_size_area(max_size = 15)
