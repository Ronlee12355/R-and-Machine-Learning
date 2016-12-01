setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
library(arules)
groceries<-read.transactions("groceries.csv",sep = ",")
summary(groceries)
itemFrequencyPlot(groceries,topN=20)
#使用image可视化稀疏矩阵，sample随机抽样
image(sample(groceries,100))
groceries_rule<-apriori(data = groceries,parameter =list(support=0.006,confidence=0.25,minlen=2))
summary(groceries_rule)
