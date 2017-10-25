setwd("E:/kaggle/us-mass-shootings-last-50-years")
rm(list = ls())
library(tidyverse)
library(stringr)
library(data.table)
library(maps)
library(lubridate)
library(leaflet)

shooting<-as.tibble(fread("Mass Shootings Dataset Ver 2.csv"))
glimpse(shooting)
shooting<-shooting %>% select(1:13) %>% mutate(Date=mdy(Date),year=year(Date))
shooting$Gender<-if_else(shooting$Gender=="M","Male",shooting$Gender)
shooting$Race<-if_else(str_detect(shooting$Race,"Black American or African American"),"Black",shooting$Race)
shooting$Race<-if_else(str_detect(shooting$Race,"White American or European American"),"White",shooting$Race)
shooting$Race<-if_else(str_detect(shooting$Race,"Asian American"),"Asian",shooting$Race)
shooting$Race<-if_else(str_detect(shooting$Race,"Some other race"),"Other",shooting$Race)
shooting$Race<-if_else(str_detect(shooting$Race,"Native American or Alaska Native"),"Native American",shooting$Race)
shooting$yearcut<-cut(shooting$year,breaks = 10)
shooting$`Mental Health Issues`<-if_else(str_detect(shooting$`Mental Health Issues`,"Un"),"Unknown",shooting$`Mental Health Issues`)
shooting$Race<-str_to_upper(shooting$Race)
shooting$`Mental Health Issues`<-str_to_upper(shooting$`Mental Health Issues`)

#查看每年死亡人数变化趋势
shooting %>% group_by(year) %>% summarise(total=sum(`Total victims`)) %>% ggplot(aes(x=year,y=total))+
  geom_bar(stat = "identity",fill="blue")+geom_text(aes(label=total),vjust=-0.2)+xlim(c(1969,2020))+geom_line(color="red")+ylab("Total victims every year")+
  ggtitle("People died because of gun shoot every year")

#发生枪击案的地点和死亡人数
shooting %>% select(`Total victims`,Fatalities,Longitude,Latitude,Summary) %>% na.omit() %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
  fitBounds(-124, 30, -66, 43) %>% addCircles(color="#8A0707", lng = ~Longitude, lat = ~Latitude, weight = 1, 
                                              radius = ~sqrt(`Total victims`) * 20000, popup = ~Summary)

shooting %>% select(`Total victims`,Fatalities,Longitude,Latitude,Summary) %>%na.omit()%>%leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
  fitBounds(-124, 30, -66, 43) %>% addCircles(color="blue", lng = ~Longitude, lat = ~Latitude, weight = 1, 
                                              radius = ~sqrt(Fatalities) * 40000, popup = ~Summary)

#杀人和性别的关系
shooting %>% ggplot(aes(x=factor(Gender),fill=factor(Gender)))+geom_bar()+labs(x="Gender",y="Number of each gender",title="The distribution of gender",fill="Gender")

#人种和发生案件的频率以及死亡人数分布
shooting %>% na.omit() %>% group_by(Race) %>% summarise(num=sum(`Total victims`)) %>% ggplot(aes(x=factor(Race),y=num,fill=factor(Race)))+geom_bar(stat = "identity")+
  coord_polar(theta = "y")+labs(x="Race",y="Number of killed people",fill="Race")+ggtitle("People killed by different races")

#不同性别的杀人分布
shooting %>% na.omit() %>% ggplot(aes(x=factor(Gender),y=`Total victims`,fill=factor(Gender)))+geom_boxplot()+ylim(c(0,90))
shooting %>% mutate(month=month(Date)) %>% group_by(month) %>% summarise(n=sum(`Total victims`)) %>% ggplot(aes(x=factor(month),y=n))+geom_bar(stat = "identity")+
  labs(x="Month",title="The distribution of killed people every month",y="Number of killed people")+geom_text(aes(label=n),vjust=-0.2,color="red")+theme_bw()

#精神疾病和死亡人数之间，枪击案发生的关系
shooting %>% na.omit() %>% ggplot(aes(x=`Mental Health Issues`)) + geom_bar()+scale_x_discrete(limits=c("NO","YES"))+theme_bw()
shooting %>% na.omit() %>% group_by(`Mental Health Issues`) %>% summarise(n=sum(`Total victims`)) %>% ggplot(aes(x=factor(`Mental Health Issues`),y=n,group=1))+
  geom_bar(stat = "identity",fill="pink")+scale_x_discrete(limits=c("NO","YES"))+geom_text(aes(label=n),vjust=-0.2)+geom_line(color="red")

#不同时间段枪击案的人种变化
shooting %>% na.omit() %>% group_by(yearcut) %>% ggplot(aes(yearcut,fill=Race))+geom_bar(position = "dodge")

#将location分成city和state两个变量
shooting$city<-sapply(shooting$Location,function(x){
  return(unlist(str_split(x,","))[1] %>% str_trim())
})
shooting$state<-sapply(shooting$Location,function(x){
  return(unlist(str_split(x,","))[2] %>% str_trim())
})
#每个城市和州发生枪击案的次数
shooting %>% group_by(city) %>% summarise(count=n()) %>% filter(city!="" & count>=2) %>% ggplot(aes(x=reorder(city,count),y=count))+
  geom_bar(stat = "identity",fill="lightblue")+coord_flip()+labs(y="Number of gun-shot happended",x="City",title="The number of case happened in each city")
shooting %>% group_by(state) %>% summarise(count=n()) %>% filter(state!="" & count>=2) %>% ggplot(aes(x=reorder(state,count),y=count))+
  geom_bar(stat = "identity",fill="lightblue")+coord_flip()+labs(y="Number of gun-shot happended",x="State",title="The number of case happened in each state")

#提取年龄
tem <- mutate(shooting,age=str_extract_all(shooting$Summary,pattern="(,\\s)\\d{2}(,)"),age2=str_extract_all(shooting$Summary,pattern="(a\\s)\\d{2}(-year)"))
tem$age<- str_sub(tem$age,3,4)
tem$age2<- str_sub(tem$age2,3,4)
te <- subset(tem,tem$age!="ar")
te2 <- subset(tem,tem$age2!="ar")
te <- rbind(te,te2)
for(i in 1:nrow(te)) 
  if(te$age[i]=="ar") te$age[i] <- te$age2[i]
te <- arrange(te,age)
te<- te[-c(1:4),]#Remove the non-digital part
te <- arrange(te,`S#`)
te$age <- as.integer(te$age)
te3 <- te %>% select(`S#`,age) %>% mutate(agecut=cut(te$age,breaks = 10*(1:7))) #Segment the age
shoot_age <- left_join(te3,shooting)

#观察年龄分布
ggplot(shoot_age,aes(agecut))+geom_bar(fill="blue")+theme_bw()
ggplot(shoot_age,aes(agecut,fill=`Mental Health Issues`))+geom_bar()
