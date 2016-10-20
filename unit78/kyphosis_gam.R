library(gam)
data(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
out.pred = predict(out,se.fit=TRUE,type='response')
plot(out,se=T)

attach(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
Xtest = ??
out.pred = predict(out,Xtest,type='response')

