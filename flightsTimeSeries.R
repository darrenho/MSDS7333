pkgs = c(
  "dplyr", "gapminder", "ggplot2", "jsonlite", "Lahman", 
  "lubridate", "modelr", "nycflights13", "purrr", "readr", 
  "stringr", "tibble", "tidyr"
)
install.packages(pkgs)

require(nycflights13)
require(ggplot2)
require(dplyr) 

not_cancelled = flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    avg_delay = mean(dep_delay)
  ) %>%
  ggplot(mapping = aes(x = avg_delay)) + 
  geom_histogram(binwidth = 5)+coord_cartesian(ylim = c(0, 50))

flight_avg_delay = not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(avg_delay = mean(dep_delay))#MAYBE A DIFFERENT STATISTIC? (MEDIAN, MAXIMUM,90TH PERCENTILE,...)

ggplot(data=flight_avg_delay,mapping = aes(x=1:365,y = avg_delay)) +
  geom_point() + 
  geom_smooth(se = FALSE)

#Get dates for the year into date data structure
tmp              = apply(select(flight_avg_delay,year,month,day),1,paste,collapse='/')
dates            = as.Date(tmp,format="%Y/%m/%d")
flight.processed = flight_avg_delay$avg_delay
#plot(flight.processed,type='l')

#flags
logdata  = FALSE
seasonal = 'weekly'

if(logdata){
  Y = log(flight.processed+abs(min(flight.processed))+0.5)
} else{
  Y = flight.processed
}
Y.ts = ts(data=Y,start=1,frequency=7)

# Analysis
if(seasonal == 'weekly'){
  #s.window='periodic' means use the same window as frequency attribute for Y.ts
  out.stl = stl(Y.ts,s.window='periodic',s.degree=0,t.window=6)
  #dim(out.stl$time.series)
  #colnames(out.stl$time.series)
  TT      = out.stl$time.series[,1]
  SS      = out.stl$time.series[,2]
  epsilon = out.stl$time.series[,3]
}
if(is.null(seasonal)){
  ## no seasonality component:
  X  = 1:365
  TT = loess(Y~X)
  epsilon = TT$residuals
}

plot(out.stl,range.bars=FALSE)

#Go To Augemented Dickey Fuller Commentary
require(tseries)
adf.test(epsilon, alternative = "stationary",k=3)
plot(dates,epsilon,type='l')


par(mfrow=c(2,1))
acf(epsilon)
pacf(epsilon,xlim=c(0,25))
dev.off()

#Forecast epsilon
require(forecast)
arma.out    = forecast:::Arima(epsilon,order=c(1,0,1))
epsilon.hat = forecast(arma.out,h=14)
plot(epsilon.hat)
Yhat          = epsilon.hat
out.forecast  = forecast(out.stl)
Yhat$mean     = Yhat$mean+rep(out.forecast$seasonal,2)
Yhat$upper    = Yhat$upper+rep(out.forecast$seasonal,2)
Yhat$lower    = Yhat$lower+rep(out.forecast$seasonal,2)
#The shift is due to a year not being an integer number of weeks
Yhat$x[2:365] = Yhat$x[2:365] + rep(out.forecast$seasonal,52)
plot(Yhat,main='epsilon and seasonal forecast')

#We can get all three components forecasted at the same time:

plot(out.forecast)

###
# ADF test exploration
#   Important point: ADF tests for presence of a "unit root" in an AR model.
#                    which is a type of nonstationarity 
###
n = 365
magGrid = 0:100
powerTest = rep(0,length(magGrid))
magSweep = 0
set.seed(1)
noiseTerm = rnorm(n)
nonlinearity = 3

for(magnitude in magGrid){
  magSweep = magSweep + 1
  tmp = magnitude*((1:n)/n-.5)**nonlinearity + noiseTerm
  p.value = adf.test(tmp, alternative = "stationary")$p.value
  powerTest[magSweep] = p.value
}
par(mfrow=c(2,1))
plot(magGrid,powerTest,type='l')
abline(h=0.05,col='red')
lastRejection = which.max(magGrid[powerTest < 0.05])
plot(lastRejection*((1:n)/n-.5)**nonlinearity + noiseTerm,type='l',ylab='Y')




