library(gam)
data(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
out.pred = predict(out,se.fit=TRUE,type='response')
plot(out,se=T)

attach(kyphosis)
out = gam(Kyphosis~s(Age,3)+ s(Number,3),family=binomial,data=kyphosis)
Xtest = data.frame('Age'=10,'Number'=4) #Age = 10, Number  = 4

##
# Three ways of generating output
##
predict(out,Xtest,type='response')#prints predictions but doesn't save 
out.pred = predict(out,Xtest,type='response')#saves predictions but doesn't print
(out.pred = predict(out,Xtest,type='response'))#does both

### Compare this to logistic regression:
###    glm(Kyphosis~Age + Number,family=binomial,data=kyphosis)