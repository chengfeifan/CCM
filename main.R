main<-function(x,y,maxLag=20,timestep=1,partion=0.5){
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
}
