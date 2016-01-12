#caculate the distance between the matrix
#matrix A
#matrix B
euclidean<-function(A,B){
  x<-try(sum((A-B)^2))
  if(inherits(x,'try-error')){
    print('The dimension between two matrix does not match')
  }
  else{
    return(x)
  }
}