setwd('/Users/darrenho/Box Sync/teaching/MSDS7333/Units11and12/code')
totalTime = 3*60

load(file='arrivals.Rdata')
load(file='numberInRestaurant.Rdata')


Lhat   = mean(numberInRestaurant)
lamHat = 1/mean(arrivals)
muHat  = lamHat*(Lhat + 1)/Lhat

#How do we get a confidence interval for muHat?
# NOTE: This is like a bootstrap, but with using the underlying distribution
arrivalF = function(lam,nReps=1000){
  results = rep(0,nReps)
  for(reps in 1:nReps){
    iter   = 0
    arrivals  = numeric()
    while(sum(arrivals) < totalTime & iter <= 10000){
      iter = iter + 1
      birth1 = rexp(1, rate = lam)
      arrivals = c(arrivals,birth1)
    }
    results[reps] = mean(arrivals)
  }
  return(results)
}

results = arrivalF(lamHat)
muTilde = (1/results)*(Lhat+1)/Lhat

hist(muTilde)
cat('95% CI [lower,upper]',quantile(muTilde,0.025),quantile(muTilde,0.975),'\n')

