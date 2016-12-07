setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
mushroom<-read.csv(file = "mushrooms.csv",stringsAsFactors = T)
str(mushroom)
mushroom<-mushroom[-17]
table(mushroom$type)
install.packages("RWeka")
Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jdk1.8.0_111')
library(RWeka)
mushroom_1R<-OneR(type~.,data=mushroom)

#to improve the ability of model
mushroom_jrip<-JRip(type~.,data=mushroom)
