# function: to standize the data

standize<-function(x,k=2){
  if(k==1){
    addSmall<-10e-16
    meanX<-mean(x)
    stdX<-sd(x)
    return((x-meanX)/stdX)
  }
  if(k==2){
    minX<-min(x)
    maxX<-max(x)
    return((x-minX)/(maxX-minX))
  }
}