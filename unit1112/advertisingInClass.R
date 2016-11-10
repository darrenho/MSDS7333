##
# Lecture example
##
nImpressions = 1000
contingencyTable = matrix(c(8,nImpressions-8,5,nImpressions-5),nrow=2,byrow=TRUE)
contingencyTable
fisher.test(contingencyTable)$p.value
chisq.test(contingencyTable)$p.value

## Elicit size of test
alpha = 0.01

# Specify the number of impressions
nImpressions = 1000

## 
clickProbA = .005
clickProbB = .006

A = rbinom(1,nImpressions,clickProbA)
B = rbinom(1,nImpressions,clickProbB)

contingencyTable = matrix(c(A,nImpressions-A,B,nImpressions-B),nrow=2,byrow=TRUE)

fisher.test(contingencyTable)$p.value
chisq.test(contingencyTable)$p.value

#####
## Power analysis:      1. How many impressions do I need to pay for to get an 80% chance of
## P(reject H_0 | H_A)     detecting a 0.5% increase in click-thru-rate for keywords in B?
##
##                      2. Is the Fisher exact test or Chi Squared test more powerful?
#####

####let's make a function called powerF that computes the power over a grid of impressions:
#  powerF = function(nGrid, clickProbA, clickProbB,alpha,nReplications = 10)

####Open up space to save our results   
#results = list()
#results[['fisher']] = matrix(0,nrow=nReplications,ncol=length(nGrid))
#colnames(results[['fisher']]) = as.character(nGrid)
#results[['chiSq']]  = matrix(0,nrow=nReplications,ncol=length(nGrid))  
#colnames(results[['chiSq']]) = as.character(nGrid)

###Loop over the impressions
#nIter = 0
#for(nImpressions in nGrid){
#nIter = nIter + 1

###Loop over replications
### (replications are for resampling the from the binomial distribution to get Monte Carlo estimate of power)
#for(r in 1:nReplications){
#A = rbinom(1,nImpressions,clickProbA)
#B = rbinom(1,nImpressions,clickProbB)
#contingencyTable = matrix(c(A,nImpressions-A,B,nImpressions-B),nrow=2,byrow=TRUE)

### Save results      
#results[['fisher']][r,nIter] = fisher.test(contingencyTable)$p.value < alpha
#if(contingencyTable[1,1] == 0 | contingencyTable[2,1] == 0){
#contingencyTable = contingencyTable + 0.5
#}
#results[['chiSq']][r,nIter]  = chisq.test(contingencyTable)$p.value < alpha  

### call function
#out = powerF(nGrid=(1:10)*1000,clickProbA = 0.005,clickProbB = 0.01,alpha,nReplications=100)

### Get Monte Carlo estimate of probabilities
# lapply(out,colMeans)

#####
## Size analysis:          1. Does the Fisher exact test or Chi Squared test have the correct size?
## P(no reject H_0 | H_0)
##  -> Which test has largest P(no reject H_0 | H_0) 
##        - note that this should be >= 1-alpha for a well designed hypothesis test
##    (Can you use your code for the previous answer for this part as well?)
#####
