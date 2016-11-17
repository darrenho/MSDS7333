setwd('/Users/darrenho/Box Sync/teaching/MSDS7333/Units11and12/code')
totalTime = 3*60

load(file='arrivals.Rdata')
load(file='numberInRestaurant.Rdata')

pdf('../ecdfArrivals.pdf')
plot(ecdf(arrivals),cex=.2)
dev.off()

#compare to standard normal

pdf('../ecdfArrivalsAndNormal.pdf')
plot(ecdf(arrivals),cex=.2)
lines(seq(-1,4,length=1000),
      pnorm(seq(-1,4,length=1000),mean(arrivals)),lty=2,col='red')
dev.off()
out_ks = ks.test(arrivals,pnorm,mean(arrivals))
print(out_ks)
#compare to exponential

pdf('../ecdfArrivalsAndExp.pdf')
plot(ecdf(arrivals),cex=.2)
lines(seq(0,4,length=1000),
  pexp(seq(0,4,length=1000),1/mean(arrivals)),lty=2,col='red')
dev.off()

out_ks = ks.test(arrivals,pexp,1/mean(arrivals))
print(out_ks)

