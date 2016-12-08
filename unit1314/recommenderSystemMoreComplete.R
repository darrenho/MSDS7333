rm(list=ls())
####
# Recommender Systems: Using matrix factorisations
#    Send me the following:
#       1) Tell me two places in the code that parallelism (not just map reduce...) could be used
#       2) Extend/Implement map reduce from last lecture for a part of the below process
####

n_row = 4
n_col = 6
m = matrix(0,nrow=n_row,ncol=n_col)

# implicit feedback data matrix.
m[1,] = c(1,1,0,0,0,0)
m[2,] = c(0,0,1,1,1,0)
m[3,] = c(1,0,1,1,0,1)
m[4,] = c(1,0,0,1,1,1)

###
# Alternating Least Squares
###
set.seed(1)

d = 2

# initial users matrix.
u = matrix(nr=n_row, nc=d, data=0);
# initial items matrix, this needs to be randomly set
v = matrix(nr=n_col, nc=d, data=rnorm(n_col*d));

objectiveF = function(u,v,m){sum(((u %*% t(v)) - m)^2)}

# find u,v to minimize:
# sum((M[i,j] - u[i,] %*% v[j,])^2 )
# 
maxIter = 10
tol = 1e-5
noConverge = TRUE
iters = 0
objectiveOld = Inf
while(noConverge & iters < maxIter) {
  iters = iters + 1
  # update users
  for(i in 1:n_row) {
    u[i,] = solve(t(v) %*% v) %*% t(v) %*% m[i,]# We can do instead: qr.solve(v, m[i,])
  }
  # update items
  for(j in 1:n_col) {
    v[j,] = solve(t(u) %*% u) %*% t(u) %*% m[,j]
  }
  # print the objective so we can see it decrease.
  objective = objectiveF(u,v,m)
  print(objective)
  if(objectiveOld-objective < tol){
    noConverge=FALSE
  }else{objectiveOld = objective}
}

(mHat_als = u %*% t(v)) # notice the inner product..
m

thresh = .1
1*(mHat_als > thresh) - m #multiplying by 1 converts TRUE/FALSE to 1/0

##
# Alternatively: SVD
##
if(!require(irlba,warn=FALSE)){install.packages('irlba');require(irlba)}

udvT = irlba(m,nv=d,nu=d)


mHat_svd = ...

mHat_als
m
mHat_svd

thresh = .1
1*(mHat_als > thresh) - m
1*(mHat_svd > thresh) - m
m

###
# Approximate SVD
###
l = d + 1 #tend to set it just slightly larger than desired dimension d

approx_svdF = function(m,l){
  #Only try to understand this function once you have learned everything else in this code
  #Resource: page 121, http://amath.colorado.edu/faculty/martinss/Pubs/2012_halko_dissertation.pdf
  n_row = nrow(m)
  n_col = ncol(m)
  set.seed(10)
  G     = matrix(rnorm(l*n_col),ncol=l)
  H     = m %*% G #this reduces the size to ease computations
  
  qr_H  = qr(H)
  Q     = qr.qy(qr_H,diag(1,n_row))[,1:l]
  J       = t(m) %*% Q
  svd_tmp = svd(J)
  udvT_approx = list('u'=Q %*% svd_tmp$v[,1:d],'v' = svd_tmp$u[,1:d])
  return(udvT_approx)
}
udvT_approx = approx_svdF(m,l)


mHat_svd_approx = udvT_approx$u %*% t(udvT_approx$v) 

mHat_als
mHat_svd
mHat_svd_approx

1*(mHat_als > thresh) - m
1*(mHat_svd > thresh) - m
1*(mHat_svd_approx > thresh) - m
m


####
# Using map reduce, parallelize some part of the above code
####


if(!require('foreach',warn=FALSE)){install.packages('foreach');require('foreach')}
if(!require('doParallel',warn=FALSE)){install.packages('doParallel');require('doParallel')}

num_cores = detectCores() - 1
registerDoParallel(num_cores)

mapF = function(mapReduceData){
  mat1Obj = mapReduceData$mat1Obj
  mat2Obj = mapReduceData$mat2Obj
  map    = mapReduceData$map
  foreach(indx = 1:nrow(map), .combine = c) %do% 
    drop(...)  
}
map = expand.grid(1:n_row,1:n_col)
mapReduceData = list('mat1Obj'=udvT_approx$u,'mat2Obj'=udvT_approx$v,'map'=map)

mapOut = mapF(mapReduceData)# this is just to check, you don't need to compute this object

reduceF = function(x,n_row){
  matrix(x,...,byrow=...)
}
(mHat_svd_approx_par = reduceF(mapF(mapReduceData),...))
mHat_svd_approx


stopImplicitCluster()
