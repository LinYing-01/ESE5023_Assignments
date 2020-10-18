x<-sample(1:100,1)
Least_moves<-function(x){
  for(i in 1:7){
    if(x==2^i){
      m=i
    }
    else if(2^i>x){
      m=i-1+x-2^(i-1)
      # misunderstanding of this problem
      # as a example of 55,the Least_move route must be "55>54>27>26>13>12>6>3>2>1", in 9 steps
      # so halving is the fastest way to approaching 1, halving is the best option whenever it can be chosen
      # rewrite this problem again
      break
    }
  }
  return(m)
}
print(Least_moves(x))

