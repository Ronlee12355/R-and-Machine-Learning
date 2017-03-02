setwd("E:/deep_learing_R/Machine-Learning-with-R-datasets-master")
library(tm)
Sys.setlocale(category = "LC_ALL", locale = "us")
library(wordcloud)
sms_raw<-read.csv(file="sms_spam.csv",stringsAsFactors = F)
sms_raw$type<-factor(sms_raw$type)
sms_corpus<-Corpus(VectorSource(sms_raw$text))#建立词库
corpus_clean<-tm_map(corpus_clean,toupper)
corpus_clean<-tm_map(sms_corpus,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
wordcloud(corpus_clean,min.freq = 30,random.order = F,random.color = F)

spam<-subset(sms_raw,sms_raw$type=="spam")
ham<-subset(sms_raw,sms_raw$type=="ham")
wordcloud(spam$text,max.words = 50)
wordcloud(ham$text,max.words = 50)

sms_dam<-DocumentTermMatrix(corpus_clean)
sms_raw_train<-sms_raw[1:4181,]
sms_raw_test<-sms_raw[4182:5574,]
sms_dam_train<-sms_dam[1:4181,]
sms_dam_test<-sms_dam[4182:5574,]
sms_dict<-findFreqTerms(sms_dam_train,5)
sms_train<-DocumentTermMatrix(corpus_clean[1:4181], control =list(dictionary = sms_dict))
sms_test<-DocumentTermMatrix(corpus_clean[4182:5574], control =list(dictionary = sms_dict))

convert<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels=c("no","yes"))
  return(x)
}
sms_train<-apply(sms_train,MARGIN = 2,convert)
sms_test<-apply(sms_test,MARGIN = 2,convert)

model<-naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
pred<-predict(model,sms_test,type = "raw")
confusionMatrix(pred,sms_raw_test$type)
plot.nb<-roc(response=sms_raw_test$type,predictor=pred[,2],levels=levels(sms_raw_test$type))
plot(plot.nb,type = "S",col="red")
