# x1 <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
# x2 <- c("S","M","M","S","S","S","M","M","L","L","L","M","M","L","L")
# y <- c(-1,-1,1,1,-1,-1,-1,1,1,1,1,1,1,1,-1)
# dat <- cbind(x1,x2,y)
# dat <- as.data.frame(dat)
# dat$x1 <- as.numeric(dat$x1) #from factor to numeric
# new_data=sample_n(dat,6)
# new_data$y=NULL
customNaiveBayes<-function(dat = NA,x=NA,Y=NA,laplace = 0){
  if(is.na(dat) || is.na(x) || is.na(Y)){
    stop("data set should not be empty!")
  }
  #获取每一个类的频数
  indicator<-table(dat[,Y])
  #获取每一个类
  class1<-unique(dat[,Y])[1];class2<-unique(dat[,Y])[2]
  
  x=unlist(x)
  freq_table<-matrix(ncol = 3)
  #统计似然频数
  for(i in x){
    freq_table<-rbind(freq_table,as.matrix(aggregate(x=dat[,Y],by=dat[c(i,Y)],FUN=length)))
  }
  freq_table<-as.data.frame(na.omit(freq_table))
  colnames(freq_table)<-c("feature","indicator","count")
  freq_table$count<-as.integer(freq_table$count)
  
  freq_table$freq<-ifelse(freq_table$indicator == class1,
                          freq_table$count/indicator[as.character(class1)],
                          freq_table$count/indicator[as.character(class2)])
  #返回结果，包括频数和类别
  out=list()
  out[['model']]<-freq_table
  out[['class']]<-indicator
  attr(out,"type")<-"NaiveBayes"
  return(out)
}

predict.navie.bayes<-function(model=NA,new_data=NA){
  if(is.na(model) || is.na(new_data)){
    stop("data set should not be empty!")
  }
  if(attributes(model)$type != "NaiveBayes"){
    stop("Invalid model")
  }
  indica<-model$class/sum(model$class)
  freq_table<-model$model
  prob.matix<-matrix(0,ncol = 2,nrow=nrow(new_data))
  classes<-names(indica)
  #计算后验概率
  result<-apply(as.matrix(new_data),1,function(x){
    cs1<-indica[classes[1]]
    cs2<-indica[classes[2]]
    for(i in 1:ncol(new_data)){
      cs1<-cs1 * freq_table$freq[freq_table$feature == x[i] & freq_table$indicator == classes[1]]
      cs2<-cs2 * freq_table$freq[freq_table$feature == x[i] & freq_table$indicator == classes[2]]
    }
    return(c(cs1,cs2))
  })
  
  result<-sapply(as.data.frame(result),function(x){
    return(x/sum(x))
  })
  rownames(result)<-classes
  #返回结果为每一个样本是不同类别的概率
  return(as.data.frame(t(result)))
}