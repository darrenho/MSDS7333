#! /usr/bin/env Rscript #allows script to be Executable 

##
# Warm-up: Experiment with sapply and lapply
#   -both take a vector or list as input and evaluates the function on each element
#   - sapply simplifies the output if possible 
#     (e.g. if the output is a list of numbers it will make it a vector of numbers)
##

# Try to think about what these will do before running them
lapply(1:3, function(x) c(x, 2*x, 4*x))
sapply(1:3, function(x) c(x, 2*x, 4*x))

###
# Intro example: creating a sentence
###

# Using lapply, make a list of sentences that are "hear no evil" "speak no evil" "see no evil"
#  - "map" the changing words like "hear" to the fixed phrase "no evil"
mapOut = lapply(c('hear','speak','see'),function(x) paste(x,'no evil',collapse=' '))

#Now, suppose we want to "reduce" this to a single sentence with commas
redOut = do.call(paste,c(mapOut,sep=', '))


###
# Statistics example: Getting predictions on a test set
#   Suppose we have estimated a coefficient vector: bHat
#   We have 1000 test observations on which we would like predictions
#       (below I'm just simulating the objects so we don't have to transfer files nor load)
#   We want to break the matrix product X %*% bHat into parts, distribute them to processors, and recombine them
#     This is going to look like (X, bhat) 
#                           map -> X[,indx] %*% bHat[indx], ... , X[,indx] %*% bHat[indx]
#                        reduce -> X[,indx] %*% bHat[indx] +  ... + X[,indx] %*% bHat[indx] = X %*% bHat
###
set.seed(1)
n = 1000
p = 100
X = matrix(rnorm(n*p),nrow=n,ncol=p)

bHat = rnorm(p) 

tol = 1e10-14#for easy vector comparison 

### Fast memory, single core way
yHat = X %*% bHat 
#alternatively 
yHat_noLinearAlgebra = apply(t(t(X)*bHat),1,sum)

max(abs(yHat-yHat_noLinearAlgebra)) < tol

### mapReduce way, but still fast memory, single core
chunkSize = 20
indx  = 1:p
map   = split(indx, ceiling(indx/chunkSize))

mapF = function(mapReduceData){
  X    = mapReduceData$X
  bHat = mapReduceData$bHat
  sapply(mapReduceData$map,function(indx){X[,indx]%*%bHat[indx]})
}

reduceF = function(x){
  sum(x)
}

mapReduceData = list('X'=X,'map'=map,'bHat'=bHat)

yHat_mr = apply(mapF(mapReduceData),1, reduceF)

max(abs(yHat-yHat_mr)) < tol

### mapReduce way, still fast memory, but now multi core
if(!require(parallel)){install.packages('parallel');require(parallel)}
num_cores = detectCores() - 1
cl        =  makeCluster(num_cores)

  #Detour: Warm- up, Redux.  
  #    Study this code and consider what each line is doing, how are they the same, how are they different
  sapply(1:3, function(x) c(x, 2*x, 4*x))
  parSapply(cl,1:3, function(x) c(x, 2*x, 4*x))

mapF = function(mapReduceData){
  X    = mapReduceData$X
  bHat = mapReduceData$bHat
  parSapply(cl,mapReduceData$map,function(indx){X[,indx]%*%bHat[indx]})
}

reduceF = function(x){
  sum(x)
}
yHat_mr = apply(mapF(mapReduceData),1, reduceF)

max(abs(yHat-yHat_mr))

stopCluster(cl)



#The foreach function can be viewed as being a more controlled version
#of parSapply that allows combining the results into a suitable 
#format. By specifying the .combine argument we can choose how to combine 
#our results.

if(!require('foreach',warn=FALSE)){install.packages('foreach');require('foreach')}
if(!require('doParallel',warn=FALSE)){install.packages('doParallel');require('doParallel')}

num_cores = detectCores() - 1
registerDoParallel(num_cores)

  #Detour: Warm- up, Redux.  
  #    Study this code and consider what each line is doing, how are they the same, how are they different
  sapply(1:3, function(x) c(x, 2*x, 4*x))
  foreach(x = 1:3,.combine=c) %do%
    c(x, 2*x, 4*x)
  #How can I make the output the same (hint: combine column-wise)
  foreach(x = 1:3,.combine=cbind) %do%
    c(x, 2*x, 4*x)

mapF = function(mapReduceData){
  X    = mapReduceData$X
  bHat = mapReduceData$bHat
  foreach(indx = mapReduceData$map, .combine = cbind) %do% 
    drop(X[,indx] %*% bHat[indx])
}
mapOut = mapF(mapReduceData)

reduceF = function(x){
  sum(x)
}
yHat_mr = apply(mapF(mapReduceData),1, reduceF)

max(abs(yHat-yHat_mr))

stopImplicitCluster()


###
# Statistics example, part 2: Getting prediction intervals
#   Suppose we have everything as above, but we also want prediction intervals
#   When we estimated bHat, we got (X^T X)^(-1) and s^2   [say from the normal equations]
#       to get a prediction interval at a point X_0, we need to compute 
#          - Prediction variance = s^2 + s^2 * X_0^T (X^T X)^(-1) X_0
###

sSq    = 1
XtXinv = solve(t(X) %*% X)

X_0 = X[1,]
### Fast memory, single core way
predVar = sSq + sSq *( t(X_0) %*% XtXinv %*% X_0)

### mapReduce way with multi core
if(!require(parallel)){install.packages('parallel');require(parallel)}
num_cores = detectCores() - 1
cl        =  makeCluster(num_cores)

mapF = function(mapReduceData){
  matObj = mapReduceData$matObj
  vecObj = mapReduceData$vecObj
  #parSapply(cl,mapReduceData$map,function(indx){XtXinv[,indx]%*%X_0[indx]})
  parSapply(cl,mapReduceData$map,function(indx){matObj[,indx]%*%vecObj[indx]})
}

reduceF = function(x){
  sum(x)
}

mapReduceData = list('matObj'=XtXinv,'vecObj'=X_0,'map'=map)

firstMR = apply(mapF(mapReduceData),1, reduceF)

mapReduceData = list('matObj'=matrix(X_0,nrow=1),'vecObj'=firstMR,'map'=map)

secondMR = reduceF(mapF(mapReduceData))

predVar_mr = sSq + sSq * secondMR
max(abs(predVar-predVar_mr)) < tol

stopCluster(cl)



#####
# Extras
#####

if(!require(mapReduce)){
  system('wget https://cran.r-project.org/src/contrib/Archive/mapReduce/mapReduce_1.2.6.tar.gz')
  install.packages('mapReduce_1.2.6.tar.gz',repos=NULL,type='source')
  require(mapReduce)
}

#Generally works like: apply(map(data), reduce)

data('iris')
mapReduce(
  map=Species, 
  mean.sepal.length=mean(Sepal.Length), 
  max.sepal.length=max(Sepal.Length), 
  data = iris  
)

mapReduce(
  map  = map, 
  yHat = X %*% bHat,
  data = mapReduceData
)
