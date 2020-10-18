#2.1
x1<-sample(0:50,50,replace=T)
x1
M1<-matrix(x1,ncol=10,nrow=5)
M1
x2<-sample(0:50,50,replace=T)
x2
M2<-matrix(x2,ncol=5,nrow=10)
M2
#2.2
Matrix_multip <- function(M1,M2){
  m <- nrow(M1)
  n <- ncol(M2)
  s <- Matrix_multip(0,m,n)
  for(i in 1:m)
    for(j in 1:n)
      s[i,j] - s+sum(M1[i,]*M2[,j])
  return(s)
}
Matrix_multip
M1%*%M2
x1<-sample(0:50,50,replace=T)
x1
M1<-matrix(x1,ncol=10,nrow=5)
M1
x2<-sample(0:50,50,replace=T)
x2
M2<-matrix(x2,ncol=5,nrow=10)
M2

# good work
