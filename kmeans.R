dataset<-iris[,c(1:3)]
k=3
custonKmeans<-function(dataset,k){
  Eudist<-function(x,y){
    distance<-sqrt(sum((x-y)^2))
    return (distance)
  }
  
  rows.dataset<-nrow(dataset)
  continue.change=TRUE
  initPoint<-dataset[sample.int(rows.dataset,size = k),]
  formerPoint<-as.matrix(initPoint)
  iterPoint<-matrix(0,nrow = k,ncol = ncol(dataset))
  #记录每个点所属的类是哪一个
  cluster.matrix<-matrix(0,nrow=rows.dataset,ncol=k)
  #记录每一个点到每一个类的距离
  error.matrix<-matrix(0,nrow=rows.dataset,ncol=k)
  
  while(continue.change){
    for(i in 1:rows.dataset){#计算每个点到三个初始中心点的距离
      for(j in 1:k){
        error.matrix[i,j]<-Eudist(dataset[i,],formerPoint[j,])
      }
    }
    #将每一个点所属的类计算出来
    for(i in 1:rows.dataset){
      cluster.matrix[i,which.min(error.matrix[i,])]<-1
    }
    
    #更新新的质心位置
    for(i in 1:k){
      iterPoint[i,]<-apply(dataset[which(cluster.matrix[,i] == 1),],2,"mean")
    }
    
    for(i in 1:k){
      if(all(unname(formerPoint[i,]) == unname(iterPoint[i,])) == T){
        continue.change=FALSE
      }
    }
    
    formerPoint = iterPoint
  }
  
  out=list()
  out[["center"]]<-iterPoint
  out[["distance"]]<-error.matrix
  for(i in 1:rows.dataset){
    out[["cluster"]][i]<-which(cluster.matrix[i,] == 1)
  }
  return(out)
}
