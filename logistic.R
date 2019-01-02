logisticLR<-function(Y=NA, data=NA, alpha=0.05, maxIterNum=1000){
  if(is.na(Y) || is.na(data)){
    stop("input data should not be NA")
  }
  sigmoid <- function(z) { 1 / (1 + exp(-z))}
  response<-as.numeric(data[[Y]])-1
  X<-data
  X[[Y]]<-NULL
  X<-as.matrix(X)
  X<-cbind(intercept=0,X)
  W<-rep(0,ncol(X))
  for(i in 1:maxIterNum){
    W<- W - alpha * (t(X) %*% (sigmoid(X %*% W) - response))
  }
  return(list("weight"=W))
}

predict.logistic<-function(model=NA, new_data=NA){
  if(is.na(model) || is.na(new_data)){
    stop("input data should not be NA")
  }
  sigmoid <- function(z) { 1 / (1 + exp(-z))}
  X<-cbind(intercept=0,new_data)
  result.prob<-sigmoid(X %*% model$weight)
  result.class<-ifelse(result.prob>=0.5,1,0)
  return(list("prob"=result.prob,"class"=result.class))
}