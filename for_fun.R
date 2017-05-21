#2.提取文章数据
url<-"http://mp.weixin.qq.com/s?__biz=MzI1ODM5NTQ1Mw==&mid=2247484083&idx=1&sn=ba4f4b10af3e4d6ed45f4d04edc30980&chksm=ea099ee1dd7e17f717afffdb3a3ff82c6e4e6bd351251601f0968c792b7e7cb5cdf084fb86a8&mpshare=1&scene=23&srcid=02039mlTmLqMxQEnb4CnUrK3#rd"
web<-read_html(url,encoding = "UTF-8")
content<-web%>%html_nodes("p")%>%html_text()

#3.数据清洗
content<-grep("^\\d{1,3}\\D",content,value = T)
content<-gsub("(\\（|\\）|\\，|\\：)",",",content)
content[1]<-"1.上海,上海1,26688亿元,同比增长6.7%,人口,2415万"
content[2]<-"2.北京,北京1,24541亿元,同比增长6.7%,人口,2171万,"
content[5]<-"5.天津,天津1,17800亿元,同比增长9%,人口,1547万,"
content[6]<-"6.重庆,重庆1,17010亿元,同比增长10.7%,人口,3372万,"
dataA<-gsub("((亿元)|(同比增长)|(人口)|(万))","",content)
result1<-ldply(str_split(dataA,","),.fun = NULL)

length_of_each<-sapply(str_split(dataA,","), function(i){length(i)})
dataA[1]<-"1.上海,上海1,26688,6.7%,,2415,"
dataA[35]<-"35.温州,浙江3,5110,8%,,919,"
dataA[36]<-"36.绍兴,浙江4,4800,5%,,501,"
colnames(result1)<-c("city","province","GDP","ratio","blank1","scale","blank2")
result1<-result1[,-c(5,7)]

wh<-regexpr("[0-9]{1,3}",result1$city)
order<-substring(result1[,1],wh,wh+attr(wh,"match.length")-1)
city<-substring(result1[,1],attr(wh,"match.length")+2)
city[nchar(city)<=1]<-"宿迁"
result<-data.frame(order,city,result1)
result$city.1<-NULL

wm<-regexpr("[0-9]{1,2}",result[,3])
prov<-substring(result[,3],1,wm-1)
scope<-substring(result[,3],wm,wm+attr(wh,"match.length")-1)

result$ratio<-sub("%","",result$ratio)
result$ratio<-as.numeric(result$ratio)
result$ratio<-result$ratio/100

resultm<-data.frame(prov,scope,result)
resultm<-resultm[,c(3,4,1,2,6,7,8)]

resultm$order<-as.numeric(resultm$order)
resultm$city<-as.character(resultm$city)
resultm$prov<-as.character(resultm$prov)
resultm$scope<-as.numeric(resultm$scope)
resultm$gdp<-as.numeric(resultm$GDP)
resultm$scale<-as.numeric(resultm$scale)

resultm<-arrange(resultm,order)
resultm$order[92:100]<-92:100
