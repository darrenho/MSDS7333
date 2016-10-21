install.packages('gam')
library(gam)
data(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
out.pred = predict(out,se.fit=TRUE,type='response')
plot(out,se=T)

attach(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
Xtest = data.frame('Age'=10,'Number'=4)
out.pred = predict(out,Xtest,type='response')

attach(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
age.seq = seq(min(Age),max(Age),len=50)
num.seq = seq(min(Number),max(Number),len=50)
Xtest = expand.grid(Age = age.seq, 
                    Number = num.seq)
out.pred = predict(out,Xtest,type='response')

persp(age.seq,num.seq,
      out.pred, phi = 60, theta = 45,
      xlab = "Age", ylab = "Number")


library(rgl)
rbPal   = colorRampPalette(c('blue','red'))
colVec = rbPal(10)[as.numeric(cut(out.pred,breaks = 10))]
plot3d(Xtest$Age,Xtest$Number,out.pred, type = "l",col=colVec)
