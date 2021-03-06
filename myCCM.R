#calculate the CCM of result 
#x: time sequence 1
#y: time sequence 2
#lag: time lag
#tag:  the position of beginning to select the data
#dimension: the dimension of embedding dimension
myCCM<-function(x,y,dist=euclidean,lag,tag,dimension=2,k=2){
  xRow<-nrow(x)
  xCol<-ncol(x)
  dataS<-dataSelect(x,y,lag,tag,dimension)
  x<-dataS[[1]]
  y<-dataS[[2]]
  number<-c(1:xRow)
  yEstimate<-unlist(lapply(number,function(i){
    dataN<-kNearest(x,i)
    yN<-y[dataN[,'location']]
    u<-exp(-(dataN[,'distance']/max(data[,'distance'])))
    w<-u/sum(u)
    yE<-sum(yN*w)
    return(yE)
  }))
  corY<-cor(y,yEstimate)
  return(list(lag,corY))
}