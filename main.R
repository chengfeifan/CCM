#x: time sequence 1
#y: time sequence 2
#maxLag: the max time lag
#timestep: the interval of sampling, default value=1
#partion: the partion of data used to estimate the y

main<-function(x,y,maxLag=20,timestep=1,partion=0.1){
  tagLen<-partion*length(x)
  tag<-floor(runif(tagLen,1,length(x)))
  lagList<-seq(timestep,maxLag,timestep)
  # y cause x
  corY_X<-unlist(lapply(tlag,function(i){
    corX<-myCCM(x,y,tag=tag,lag=i)
    return(corX)
  }))
  corX<-corY_X[seq(2,length(corY_X),2)]
  
  # x cause y
  corX_Y<-unlist(lapply(tlag,function(i){
    corY<-myCCM(y,x,tag=tag,lag=i)
    return(corY)
  }))
  corY<-corX_Y[seq(2,length(corX_Y),2)]
  
  # plot the result
  opar<-par(no.readonly = TRUE)
  par(lty=2,pch=2,lwd=2)
  plot(tagLen,corX,ylim=c(0,1),col="blue",type="b",xlab="Timestep",ylab = "ro")
  lines(tagLen,corY,col="red",type="b")
  legend("topright",legend=c("y cause x","x cause y"),col=c("blue","red"),pch=2,lty=2)
  grid()
  par(opar)
  
  return(NULL)
}
