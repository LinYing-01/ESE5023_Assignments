Pascal_triangle<-function(k){
  Tri<-array(0,dim=c(k,k))
  for(i in 1:k){
    Tri[i,c(1,i)]<-1
  }
  if(k>=1){
    for(i in 3:k){
      for(j in 2:(k-1)){
        Tri[i,j]<-Tri[i-1,j-1]+Tri[i-1,j]
      }
    }
  }
  return(Tri[k,])
}
print(Pascal_triangle(100))
print(Pascal_triangle(200))

