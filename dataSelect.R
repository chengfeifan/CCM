#To select the ramdon data from the sequence x and y
#x: time sequence 1
#y: time sequence 2
#lag: Time lag
#tag: the position of beginning to select the data
dataSelect<-function(x,y,lag,tag,dimension){
  library(parallel)
  x<-standize(x)
  y<-standize(y)
  xLen<-length(x)
  yLen<-length(y)
  if(xLen!=yLen){
    print('The length does not match')
    return(NULL)
  }
  else{
    cl<-makeCluster(getOption("cl.cores",8))
    tag<-tag[which(tag+dimension*lag<xLen+1 & tag+dimension*lag>0)]
    dataRange<-parLapply(cl,tag,function(tagdot){
      xdot<-x[seq(tagdot,tagdot+dimension*lag,lag)]
      ydot<-y[tagdot+lag]
      return(list(xdot,ydot))
    })
    stopCluster(cl)
    dataX<-dataRange[[1]][[1]]
    dataY<-dataRange[[1]][[2]]
    for(i in 2:length(dataRange)){
      dataX<-rbind(dataX,dataRange[[i]][[1]])
      dataY<-rbind(dataY,dataRange[[i]][[2]])
    }
    return(list(dataX,dataY))
  }
}
