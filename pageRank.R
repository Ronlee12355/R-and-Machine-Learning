pagerank<-function(gene_vector,d_value=0.7,fppi="E:/cmap/cmap数据/String_PPI_400.txt"){#第二步
  d_value=0.7
  gene_vector=CGD
  ppi<-fread(fppi)
  ppi <- ppi %>% select(gene1,gene2) %>% filter(gene1 %in% gene_vector$gene & gene2 %in% gene_vector$gene)
  number_all_gene<-length(gene_vector$gene)
  W<-matrix(rep(0,number_all_gene*number_all_gene),nrow = number_all_gene,ncol = number_all_gene,byrow = T)
  ex=rank(gene_vector$value)
  r<-ex/sum(ex)
  colnames(W)<-gene_vector$gene
  rownames(W)<-gene_vector$gene
  for(i in 1:nrow(ppi)){
    W[as.character(ppi[i,1]),as.character(ppi[i,2])] = 1
  }
  D<-diag(apply(W, 1,sum)) %>% Matrix::Matrix(sparse = T)
  W=Matrix::Matrix(W,sparse = T)
  n=0
  repeat{
    r=(1-d_value)*ex + t(W)%*%solve(D)%*%r*d_value
    n=n+1
    print(r)
    if(n>=40){
      break
    }
  }
}
