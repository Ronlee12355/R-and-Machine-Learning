setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
customer<-read.csv("customer.csv",stringsAsFactors = F)
customer$ID<-NULL
customer<-scale(customer)
kmeans.model<-kmeans(customer,10)
clusplot(customer,kmeans.model$cluster,color = T,shade = T)
plot(customer,col=kmeans.model$cluster)

#寻找最佳k值
nk<-2:12
set.seed(22)
wss<-sapply(nk, function(k){
  kmeans(customer,centers = k)$tot.withinss
})
plot(nk,wss,type="l")
k=nk[which.min(wss)]
