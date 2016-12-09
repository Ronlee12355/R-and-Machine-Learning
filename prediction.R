#读取数据，对数据进行准备
setwd("E:/deep_learing_R/drug_pred_model")
library(e1071)
library(pROC)
library(MASS)
load("scgdrug.RDATA")
data_train<-read.table(file = "four-two.txt",sep = "\t")
colnames(data_train)<-c("x1","x2","x3","x4","class")
data_train$class<-factor(data_train$class)


#开始训练模型
bayes_model<-naiveBayes(class~.,data_train)
svm_model<-svm(class~.,data =data_train,probability=T)
glm_model<-glm(class~.,data = data_train,family = "binomial")

#读取输入数据，并计算特征值
#input<-commandArgs(T)
#drug_input<-as.character(input[1])
drug_input<-"drug1"
drug_genes<-c('UBE2Q1', 'RNF14', 'RNF17', 'RNF10', 'RNF13', 'CCDC109B', 'DUOXA2', 'MZT2A', 'MZT2B', 'ATRX', 'PMM1', 'ASS1', 'NCBP1', 'ZNF709', 'RBM141234')
#for (i in 2:ncol(input)) {
  #drug_genes[i-1]<-as.character(input[i-1])
#}
drug_genes_num<-length(intersect(drug_genes,allGeneticTargets))
if(drug_genes_num>0){
  data_input<-c()
  for (i in 1:ncol(disGenes)) {
    dis_cid<-disNames.disGenes[i]
    dis_name<-as.character(cuiToDisname[which(dis_cid==cuiToDisname[,1]),2])
    if(dis_cid %in% disNames.topGenes){
      top_gene<-union(ohnoGenes,topGenes[,which(disNames.topGenes==dis_cid)])
    }else{
      top_gene<-ohnoGenes
    }
    m1=0;m2=0.0;m3=0;m4=0.0
    a<-intersect(disGenes[,i],drug_genes)
    m1<-length(a)
    m2<-m1/drug_genes_num
    m3<-length(intersect(topGenes,a))
    m4<-m3/drug_genes_num
    if(m1!=0){
      tmp<-c(m1,m2,m3,m4,drug_input,dis_name)
      data_input<-rbind(data_input,tmp)
    }
  }
  feature<-data.frame(x1=as.integer(data_input[,1]),x2=as.numeric(data_input[,2]),x3=as.integer(data_input[,3]),x4=as.numeric(data_input[,4]))
  assoDrugDis <- data.frame(drugs=data_input[,5],diseases=data_input[,6])
  nb<-c(predict(bayes_model,feature,type = "raw")[,2])
  svm<-predict(svm_model,feature,probability = T)
  svm<-c(attr(svm,"probabilities")[,2])
  glm<-predict(glm_model,feature,type="response")
  result<-data.frame(assoDrugDis,nb,svm,glm)
  write.table(result,file = "aaa.txt",sep = "\t",row.names = F,col.names = F,quote = F)
}else{
  result<-data.frame(drugs=drug, diseases="No diseases related to this drug", bayes = 0.0, glm = 0.0, svm = 0.0)
  write.table(result,file = "aaa.txt",sep = "\t",row.names = F,col.names = F,quote = F)
}
