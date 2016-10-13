nenana = read.table('nenana.txt',header=TRUE,sep=',')

###
# Look at 3 different fitting approaches with different smoothness
#   1) Loess: c(.1,.25,.5,1)
#   2) Regression splines: c(n/100,n/25,n/10,n/2,n)
#   3) Smoothing splines: Choose amount of smoothing via GCV, 1/4,1/3,1/2 that much
#   
###

x = nenana$Year
Y = nenana$Julian.Date

# 1) Loess: can you fit Loess?

# 2) Regression Spline
require('splines')
plot(x,Y)
kSweep = 0
n      = length(x)
Kgrid  = ceiling(c(n/25,n/10,n/2,n))
colVec = rainbow(length(Kgrid))#creates different colors for plotting
for(K in Kgrid){
  kSweep = kSweep + 1
  X = bs(x,df=K)
  Yhat = predict(lm(Y~.,data=X))
  lines(x,Yhat,col=colVec[kSweep])   
}
legendVec = paste('K = ',Kgrid,sep='')
legend(x='topleft',legend=legendVec,col=colVec,lty=1)

# 3)Smoothing spline
out  = smooth.spline(x, Y, cv=FALSE)
lambdaChosenGCV = out$lambda

plot(x,Y)
lamSweep  = 0
n       = length(x)
lamGrid = c(lambdaChosenGCV/4,lambdaChosenGCV/3,lambdaChosenGCV/2,lambdaChosenGCV)
colVec  = rainbow(length(lamGrid))#creates different colors for plotting
for(lam in lamGrid){
  lamSweep = lamSweep + 1
  Yhat = fitted(smooth.spline(x, Y, spar=lam))
  lines(x,Yhat,col=colVec[lamSweep])   
}
legendVec = paste('lambda = ',lamGrid,sep='')
legend(x='topleft',legend=legendVec,col=colVec,lty=1)
