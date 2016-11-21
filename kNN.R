setwd("E:/deep_learing_R")
wbcd<-read.csv(file = "wdbc.data",sep = ",",stringsAsFactors = F)
colnames(wbcd)=c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean")
wbcd<-wbcd[,-1]
wbcd[,1]<-factor(wbcd[,1],levels = c("B","M"),labels = c("Benign","Malignant"))#将诊断结果因子化
summary(wbcd[,2:5])
wbcd_z<-as.data.frame(scale(wbcd[,-1]))#对除了第一行的其他数据集进行z-score处理
wbcd_train=wbcd_z[1:468,]#训练集
wbcd_test<-wbcd_z[469:568,]#测试集
wbcd_train_lables<-wbcd[1:468,1]
wbcd_test_lables<-wbcd[469:568,1]
library(class)
library(gmodels)
wbcd_pred<-knn(train = wbcd_train,test = wbcd_test,cl=wbcd_train_lables,k=21)#进行kNN算法处理
Cross Table(x=wbcd_test_lables,y=wbcd_pred,prop.chisp=F)
