x<-sample(1:100,1)
Least_moves<-function(x){
  for(i in 1:7){
    if(x==2^i){
      m=i
    }
    else if(2^i>x){
      m=i-1+x-2^(i-1)
      break
    }
  }
  return(m)
}
print(Least_moves(x))

