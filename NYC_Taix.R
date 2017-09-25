setwd("E:/kaggle/NYC_TAIX")
library(ggplot2)
library(dplyr)
library(data.table)
library(corrplot)
library(tibble)
library(caret)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(scales)
library(geosphere)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#检查数据完整信息
test<-as.tibble(fread("test.csv"));train<-as.tibble(fread("train.csv"))
summary(train);glimpse(train);sum(sapply(train, is.na))
summary(test);glimpse(test);sum(sapply(test, is.na))
combine<-bind_rows(train %>% mutate(dset="train"),test %>% mutate(dset="test",dropoff_datetime=NA,trip_duration = NA))
combine<-combine %>% mutate(dset=as.factor(dset))

#进行数据的初步分析
train<-train %>% mutate(dropoff_datetime=ymd_hms(dropoff_datetime),pickup_datetime=ymd_hms(pickup_datetime),
                        passenger_count=factor(passenger_count),vendor_id=factor(vendor_id))

#检查pickuptime和dropofftime的差距是否正常
train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

train %>% ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +scale_x_log10() +scale_y_sqrt()+ggtitle("主要出行时间分布")

train %>% ggplot(aes(pickup_longitude,pickup_latitude))+geom_point()+xlim(c(-80,-60))+ggtitle("数据点的分布情况")

p1<-train %>% ggplot(aes(pickup_datetime))+geom_histogram(fill="red",bins = 120)+ggtitle("主要上车时间分布")
p2<-train %>% ggplot(aes(dropoff_datetime))+geom_histogram(fill="blue",bins = 120)+ggtitle("主要下车时间分布")
multiplot(p1,p2)

p1<-train %>% group_by(passenger_count) %>% ggplot(aes(passenger_count,fill=passenger_count))+
  geom_bar()+scale_y_sqrt()+ggtitle("barplot of the number of the passerage")
p2<-train %>% mutate(week=wday(pickup_datetime,label=T)) %>% group_by(week,vendor_id) %>% count() %>% 
  ggplot(aes(x=week,y=n,color=vendor_id))+geom_point(size=5)+labs(x="days of the week",y="total counts of days")
p3<-train %>% mutate(week=hour(pickup_datetime)) %>% group_by(week,vendor_id) %>% count() %>% 
  ggplot(aes(x=week,y=n,color=vendor_id))+geom_point(size=5)+labs(x="hours of the day",y="total counts of hours")
multiplot(p1,p2,p3)

train %>% ggplot(aes(passenger_count,trip_duration,color=passenger_count))+geom_boxplot()+scale_y_log10()+facet_wrap(~vendor_id)+
  labs(x="number of passerages",y="trip_durations")

p1 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(wday, median_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median trip duration [min]")

p2 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(hpick, median_duration, color = vendor_id)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median trip duration [min]") +
  theme(legend.position = "none")
multiplot(p1, p2)

p1<-train %>% mutate(h=hour(pickup_datetime),mon=factor(month(pickup_datetime,label=T))) %>% group_by(h,mon) %>%
  count() %>% ggplot(aes(h,n,color=mon))+geom_line(size=2)
p2<-train %>% mutate(h=hour(pickup_datetime),mon=factor(wday(pickup_datetime,label=T))) %>% group_by(h,mon) %>%
  count() %>% ggplot(aes(h,n,color=mon))+geom_line(size=2)
multiplot(p1,p2)

train %>% ggplot(aes(vendor_id,fill=vendor_id))+geom_bar()+coord_polar()

p1<-train %>% mutate(h=hour(pickup_datetime)) %>% group_by(h) %>% count() %>%
  ggplot(aes(h,n))+geom_line(size=2,color="lightblue")+geom_point(size=2,shape=16)+ggtitle("每天打车时段分布")

p2<-train %>% mutate(h=wday(pickup_datetime,label=T)) %>% group_by(h) %>% count() %>%
  ggplot(aes(h,n,group=1))+geom_line(size=2,color="red")+geom_point(size=2,shape=16)+ggtitle("每周打车时段分布")

p3<-train %>% mutate(h=month(pickup_datetime,label=T)) %>% group_by(h) %>% count() %>%
  ggplot(aes(h,n,group=1))+geom_line(size=2,color="orange")+geom_point(size=2,shape=16)+ggtitle("每月打车时段分布")
multiplot(p1,p2,p3)

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)
train$dist<-distCosine(train %>% select(pickup_longitude,pickup_latitude),train %>% select(dropoff_longitude,dropoff_latitude))
train$bearing<-bearing(train %>% select(pickup_longitude,pickup_latitude),train %>% select(dropoff_longitude,dropoff_latitude))
train %>% sample_n(5e4) %>% 
  ggplot(aes(dist,trip_duration))+geom_point()+scale_x_log10()+
  scale_y_log10()+geom_smooth(method = lm)+ggtitle("等待时间和距离的关系")

train %>% mutate(speed=dist/trip_duration*3.6) %>% filter(speed>2 & speed < 1e2) %>% 
  ggplot(aes(speed))+geom_histogram(fill="red",binwidth = 50)+ggtitle("汽车速度的分布情况")

train$jfk_dist_pick <- distCosine(train %>% select(pickup_longitude,pickup_latitude),jfk_coord)
train$jfk_dist_drop <- distCosine(train %>% select(dropoff_longitude,dropoff_latitude),jfk_coord)
train$lg_dist_pick <- distCosine(train %>% select(pickup_longitude,pickup_latitude),la_guardia_coord)
train$lg_dist_drop <- distCosine(train %>% select(dropoff_longitude,dropoff_latitude), la_guardia_coord)

train<- train %>% mutate(
  vendor_id = factor(vendor_id),
  speed = dist/trip_duration*3.6,
  date = date(pickup_datetime),
  month = month(pickup_datetime, label = TRUE),
  wday = wday(pickup_datetime, label = TRUE),
  hour = hour(pickup_datetime),
  work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
  jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
  lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
  blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
)

p1<-train %>% group_by(wday,vendor_id) %>% summarise(mspeed=median(speed)) %>% ggplot(aes(wday,mspeed,color=vendor_id))+
  geom_point(size=4)+ggtitle("一周内速度的变化")

p2<-train %>% group_by(wday,hour) %>% summarise(mspeed=median(speed)) %>% ggplot(aes(hour,wday,fill=mspeed))+
  geom_tile()+ggtitle("每天速度的变化[每周的变化]")+scale_fill_distiller(palette = "Spectral")
multiplot(p1,p2)

p1 <- train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(jfk_trip, trip_duration, color = jfk_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "JFK trip")
p2 <- train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(lg_trip, trip_duration, color = lg_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "La Guardia trip")
multiplot(p1, p2)
