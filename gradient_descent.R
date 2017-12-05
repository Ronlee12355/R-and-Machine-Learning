set.seed(123)
x <- matrix(rnorm(5000), ncol = 5)
X<-cbind(rep(1, 1000), x)
y <- rnorm(1000)
θ<-rep(0,6)

#Cost Function

CostFun<-function(X, y, θ){
  return(sum((X%*%θ- y)^2)/2)
}

#Gradient Descent

GradDescent<-function(X, y, θ, alpha, i){
  hist <- rep(0, i)
  for(j in 1:i){
	θ<- θ - alpha*(t(X)%*%(X%*%θ-y))
	hist[j]<-CostFun(X,y,θ)
  }
  results<-list(hist,θ)
  return(results)
}
aa<-GradDescent(X,y,θ,0.00005,200)
