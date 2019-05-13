data(ggplot2::diamonds)
library(caret)
library(dplyr)
dia.trans<-bind_cols(diamonds %>% select_if(is.numeric),
                     model.matrix(~cut-1,diamonds) %>% as_tibble(),
                     model.matrix(~color-1,diamonds) %>% as_tibble(),
                     model.matrix(~clarity-1,diamonds) %>% as_tibble())

#setting parameters alpha and lambda
lasso_expand<-expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=dia.trans %>% select(-price), y=dia.trans$price, method='glmnet', 
                   tuneGrid=lasso_expand)

#best tune
lasso_mod$bestTune
lasso_mod$results$RMSE

lasso_imp<-varImp(lasso_mod)
#get the importance of each feature and eliminate some of them
lasso_imp$importance

df<-iris %>% select_if(is.numeric)
str(iris)
index<-sample(2,nrow(iris),replace = T,prob = c(0.7,0.3))
tr<-df[index==1,];ts<-df[index==2,]
para<-expand.grid(lambda=seq(0.05,1,0.01),alpha=1)
lasso_mod <- train(x=tr %>% select(-price), y=tr$price, method='glmnet', 
                   tuneGrid=para)
lasso_mod$bestTune
imp<-varImp(lasso_mod)
imp$importance

out=c()
lam<-seq(0.005,100,0.005)
for(i in 1:length(lam)){
  set.seed(200)
  folds<-createFolds(tr$price, k=5)
  mes<-lapply(folds, function(x){
    trains<-tr[-x,];test<-tr[x,]
    model<-glmnet(x=trains %>% select(-price) %>% as.matrix(), y=trains$price,lambda = lam[i])
    pre<-predict(model,test %>% select(-price) %>% as.matrix())
    RSS<-(pre-test$price)^2 %>% sum()
    TSS<-(trains$price-mean(trains$price))^2 %>% sum()
    return(1-RSS/TSS)
  })
  mes<-unlist(mes) %>% mean()
  out[i]<-mes
}
