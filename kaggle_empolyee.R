setwd("E:/kaggle/employee_access")
library(data.table)
library(dplyr)
raw_test<-fread('test.csv')
raw_train<-fread('train.csv')
employee<-bind_rows(fread('train.csv'),fread('test.csv'))
employee$id<-NULL
library(Amelia)
missmap(employee)
min_max<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#employee_scale<-as.data.table(apply(employee[,-1], 2, "min_max"))
#employee_scale$ACTION<-employee$ACTION
library(corrplot)
corrplot(cor(employee_scale),type="lower",method = "number")
library(randomForest)
employee$ACTION<-factor(employee$ACTION)
library(ggplot2)
hist(employee$ROLE_ROLLUP_1)
hist(employee$ROLE_ROLLUP_2)
ggplot(employee[1:32769,],aes(x=ROLE_ROLLUP_2,fill=ACTION))+geom_histogram()
employee_new=data.table()
employee_new$ROLE_ROLLUP_2<-if_else(employee$ROLE_ROLLUP_2)
ggplot(employee[1:32769,],aes(x=RESOURCE,fill=ACTION))+geom_histogram(position = "dodge")
ggplot(employee[1:32769,],aes(x=ROLE_DEPTNAME,fill=ACTION))+geom_histogram()
ggplot(employee[1:32769,],aes(x=ROLE_TITLE,fill=ACTION))+geom_histogram()
ggplot(employee[1:32769,],aes(x=ROLE_FAMILY_DESC,fill=ACTION))+geom_histogram()
ggplot(employee[1:32769,],aes(x=ROLE_FAMILY,fill=ACTION))+geom_histogram()
ggplot(employee[1:32769,],aes(x=ROLE_CODE,fill=ACTION))+geom_histogram()

#看得出数据量最大的是117961
is_117961<-if_else(employee$ROLE_ROLLUP_1==117961,1,0)
employee_new<-data.table(is_117961<-is_117961)
colnames(employee_new)<-"is_117961"

max(table(employee$ROLE_ROLLUP_2))
employee[which.max(table(employee$ROLE_ROLLUP_2))]
employee_new$is_118300_or_118343<-if_else(employee$ROLE_ROLLUP_2>=118300 & employee$ROLE_ROLLUP_2<=118386,1,0)
employee_new$is_118300_or_118343<-as.factor(employee_new$is_118300_or_118343)

table(employee$ROLE_DEPTNAME)[table(employee$ROLE_DEPTNAME)>1000]
employee_new$is_ROLE_DEPTNAME<-if_else(employee$ROLE_DEPTNAME==117878 | employee$ROLE_DEPTNAME==117941 | employee$ROLE_DEPTNAME==117945,1,0)
employee_new$is_ROLE_DEPTNAME<-as.factor(employee_new$is_ROLE_DEPTNAME)

table(employee$ROLE_TITLE)[table(employee$ROLE_TITLE)>5000]
employee_new$is_ROLE_TITLE<-if_else(employee$ROLE_TITLE==117905 | employee$ROLE_TITLE==118321,1,0)
employee_new$is_ROLE_TITLE<-as.factor(employee_new$is_ROLE_TITLE)

table(employee$ROLE_FAMILY_DESC)[table(employee$ROLE_FAMILY_DESC)>5000]
employee_new$is_ROLE_FAMILY_DESC<-as.factor(if_else(employee$ROLE_FAMILY_DESC==117906,1,0))

table(employee$ROLE_FAMILY)[table(employee$ROLE_FAMILY)>5000]
employee_new$is_ROLE_FAMILY<-as.factor(if_else(employee$ROLE_FAMILY==290919,1,0))

table(employee$ROLE_CODE)[table(employee$ROLE_CODE)>5000]
employee_new$is_ROLE_CODE<-as.factor(if_else(employee$ROLE_CODE==117908 | employee$ROLE_CODE== 118322,1,0))
employee_new$ACTION<-employee$ACTION
employee_new$MGR_ID<-employee$MGR_ID

table(employee$RESOURCE)[table(employee$RESOURCE)>1000]
employee_new$RESOURCE<-employee$RESOURCE
employee_new$ACTION[!is.na(employee_new$ACTION)]<-as.factor(employee_new$ACTION[!is.na(employee_new$ACTION)])

model<-randomForest(ACTION~.,data = employee_new[1:32769,])
predict.test<-predict(model,employee_new[32770:91690,],type = "prob")
result<-data.table(Id=raw_test$id,Action=predict.test[,2])
write.csv(result,"submission.csv",row.names = F)
