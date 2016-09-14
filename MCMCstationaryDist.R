######
#MCMC: small number of states example
######

stateSpace = c(1,2,3)
probMat    = matrix(c(0,.1,.9,.1,0.,.9,.1,.9,0),nrow=3,ncol=3,byrow=T)

##
# Get posterior via probability theory and numerical approx.
#  lim_{t -> infinity} probMat^t = posterior  (also known as stationary dist.)
##
posterior = probMat
for(j in 1:200){
  posterior = posterior %*% probMat
}

posterior = posterior[1,]#the stationary dist. is the same for all states
cat('posterior is (approx)',posterior,'\n')

#  Now, I can create all the samples I want from the posterior
#    Point of contact: each sample would be an imputed obs. in a multiple imputation

sample(stateSpace,size=1,prob=posterior)

####
# Get posterior via MCMC
#  Crucial bit: we only really need something proportional to probMat to make this work 
#    (i.e. the likelihood * prior)
####
moveF = function(st,stateSpace,probMat){
  #Takes a current state and goes to the next state according to probMat
  return(sample(stateSpace,size=1,prob=probMat[st,]))
}

nTimes = 20000


#Generates a sequence of observations from this MARKOV CHAIN
results = matrix(0,nrow=nTimes,ncol=length(stateSpace))
oldMove = 1 #initial value
for(tt in 1:nTimes){
  newMove             = moveF(oldMove,stateSpace,probMat)
  results[tt,newMove] = 1
  oldMove             = newMove
}
#Here, let's look at the estimate of the stationary dist.
cat('posterior estimated by simulation',apply(results,2,mean),'\n')
cat('posterior is (approx)',posterior,'\n')

#####
#What if we want independent draws from the posterior instead?
#####

#If we have the posterior
posteriorSample = sample(stateSpace,size=100,replace=TRUE,prob=posterior)

#If we want to use our MARKOV CHAIN draws
results[1:10,] #clearly, these aren't independent.  Also they depend on initial value

#What we do have is a TIME SERIES for each state.  
# If we are looking for independent draws, it is necessary they look like WHITE NOISE
par(mfrow=c(3,1),mar=rep(3,4))
acf(results[,1],main='State 1')
acf(results[,2],main='State 2')
acf(results[,3],main='State 3')

#let's 
#  1) Throw away first few observations (so the chain "forgets" where it started from)
resultsBurn = results[-(1:200),] #often called a "burn in"
#  2) Sub-sample by only keep every kth draw for varying k's
k = 20
if(nrow(resultsBurn)%%k != 0){
  print('not a divisor')
  break
} else{
  subSamp   = rep(c(rep(FALSE,k-1),TRUE),nrow(resultsBurn)/k)
  results_k = resultsBurn[subSamp,]
  par(mfrow=c(3,1),mar=rep(3,4))
  acf(results_k[,1],main='State 1')
  acf(results_k[,2],main='State 2')
  acf(results_k[,3],main='State 3')
}

resultsInd = results_k[1:100,]

head(resultsInd)
