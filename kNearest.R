# function: find the k nearest dot of x[i,]
# x: the embedding manifold of x
# i: x[i,]
# k: the k nearest
kNearest<-function(x,i,k=2){
  library(parallel)
  xt<-x[i,]
  cl<-makeCluster(getOption("cl.cores",8))
  distance<-parApply(cl,x,1,function(xx){
    return(euclidean(xx,xt))
  })
  location<-c(1:nrow(x))
  data<-data.frame(location,distance)
  dataSort<-data[order(data[,'distance']),]
  dataReturn<-dataSort[2:(k+1),]
  stopCluster(cl)
  return(dataReturn)
}
