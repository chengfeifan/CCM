#To select the ramdon data from the sequence x and y
#x: sequence 1
#y: sequence 2
#lag: Time lag
#tag: the position of beginning to select the data
dataSelect<-function(x,y,lag,tag){
  xLen<-length(x)
  yLen<-length(y)
  if(xLen!=yLen){
    print('The length does not match')
    return(NULL)
  }
  else{
    tag<-tag[which(tag+lag<xLen+1 & tag+lag>0)]
    dataRange<-unlist(lapply(tag,function(tagdot){
      xdot<-x[tagdot]
      ydot<-y[tagdot+lag]
      return(list(xdot,ydot))
    }))
    dataX<-dataRange[seq(1,length(dataRange),2)]
    dataY<-dataRange[seq(2,length(dataRange),2)]
    return(list(dataX,dataY))
  }
}