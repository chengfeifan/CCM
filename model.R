x1<-rep(1,10000)
x2<-rep(1,10000)
x3<-rep(1,10000)
x4<-rep(1,10000)
x5<-rep(1,10000)

for(i in 11:10000){
  x1[i]=0.6*x1[i-1]+0.4*x3[i-1]
  x2[i]=0.6*x2[i-1]+0.3*x5[i-1]
  x3[i]=0.3*x1[i-1]+0.45*x3[i-1]+0.3*x5[i-10]
  x4[i]=0.2*x1[i-10]+0.3*x2[i-1]+0.4*x4[i-1]
  x5[i]=0.4*x2[i-1]+0.5*x5[i-1]+runif(1,0,1)
}