setwd("E:/kaggle/titanic")
library(data.table)
library(dplyr)
library(Amelia)
library(Hmisc)
library(stringr)
library(randomForest)
library(caret)
raw_train<-fread("train.csv")
raw_test<-fread("test.csv")
all_data<-bind_rows(raw_train,raw_test)
missmap(all_data)
describe(all_data[,-2])

#缺失值embarked
table(all_data$Embarked,useNA = "ifany")
all_data[which(all_data$Embarked=="")]
table(all_data$Embarked[which(all_data$Pclass=="1" & all_data$Survived=="1")])#C or S
ggplot(all_data,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+geom_boxplot()
all_data$Embarked[all_data$Embarked==""]<-"C"

#缺失值Fare
table(all_data$Fare,useNA = "ifany")
all_data[which(is.na(all_data$Fare))]
ggplot(all_data[all_data$Pclass=="3" & all_data$Embarked=="S",],aes(x=Fare))+geom_density(fill="blue")
fare.matrix<-as.matrix(table(all_data$Fare[all_data$Pclass=="3" & all_data$Embarked=="S"]))
all_data$Fare[is.na(all_data$Fare)]<-median(all_data$Fare,na.rm = T)

#缺失值age
library(mice)
mice_mod<-mice(all_data[,-c('PassengerId','Name','Ticket','Cabin','Survived')],method = "rf")
mice_output<-complete(mice_mod)
all_data$Age<-mice_output$Age

#探究家庭数目和存活率的关系
all_data$fz<-all_data$SibSp+all_data$Parch+1
ggplot(all_data[1:891,],aes(x=factor(FamilySize),fill=factor(Survived)))+geom_bar(position = "dodge")
all_data$FamilySize[all_data$fz==1]<-"single"
all_data$FamilySize[all_data$fz %in% c(2,3,4)]<-"middle"
all_data$FamilySize[all_data$fz>=5]<-"big"
all_data$FamilySize<-as.factor(all_data$FamilySize)

#探究票价和存活率的关系
all_data$FareSort[all_data$Fare<=30]<-"low"
all_data$FareSort[all_data$Fare>=30 & all_data$Fare<=300]<-"fine"
all_data$FareSort[all_data$Fare>=300]<-"high"
ggplot(all_data[1:891,],aes(x=factor(FareSort),fill=factor(Survived)))+geom_bar(position = "dodge")
all_data$FareSort<-as.factor(all_data$FareSort)

#age
all_data$child[all_data$Age<5]<-"child"
all_data$child[all_data$Age>=5 & all_data$Age<18]<-"teen"
all_data$child[all_data$Age>=18]<-"adult"
ggplot(all_data[1:891,],aes(x=factor(child),fill=factor(Survived)))+geom_bar(position = "dodge")
all_data$child<-as.factor(all_data$child)

#头衔的关系
all_data$Name<-as.character(all_data$Name)
table_name<-str_split(all_data$Name,",")
table_name<-lapply(table_name, function(x){
  x<-x[-1]
  return(str_extract(x,"[a-zA-Z]+\\."))
})
table(unlist(table_name))
all_data$Title<-str_match(all_data$Name,"[a-zA-Z]+\\.")
ggplot(all_data[1:891,],aes(x=factor(Title),fill=factor(Survived)))+geom_bar(position = "dodge")
all_data$Title[all_data$Title=="Ms."]<-"Miss."
all_data$Title[all_data$Title=="Mlle."]<-"Miss."
all_data$Title[all_data$Title=="Mme."]<-"Mrs."
all_data$Title[all_data$Title=="Ms."]<-"Miss."
all_data$Title[all_data$Title=="Sir." | all_data$Title=="Countess." | all_data$Title=="Lady."]<-"honor"
all_data$Title[all_data$Title %in% c('Capt.','Col.','Don.','Dona.','Dr.','Jonkheer.','Major.','Rev.')]<-"others"
all_data$Title<-as.factor(all_data$Title)

#mother
all_data$Mother<-"no"
all_data$Mother[all_data$Title!="Miss." & all_data$Age>=18 & all_data$Sex=="female" & all_data$Parch>0]<-"yes"
ggplot(all_data[1:891,],aes(x=factor(Mother),fill=factor(Survived)))+geom_bar(position = "dodge")
all_data$Mother<-as.factor(all_data$Mother)
#没有用的cabin属性
all_data$Survived<-as.factor(all_data$Survived)
all_data$Sex<-as.factor(all_data$Sex)
all_data$Pclass<-as.factor(all_data$Pclass)
all_data$Embarked<-as.factor(all_data$Embarked)
train<-all_data[1:891,]
test<-all_data[892:1309,]
rf.model<-randomForest(Survived~Pclass+FareSort+Embarked+Sex+child+FamilySize+Title+Mother,data = train)
rf.predict<-predict(rf.model,test)
result<-cbind(test$PassengerId,rf.predict)
colnames(result)<-c("PassengerId","Survived")
result<-as.data.frame(result)
result$Survived<-ifelse(result$Survived=="1",0,1)
write.csv(result,"submission.csv",row.names = F)
