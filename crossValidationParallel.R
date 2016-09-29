#https://gist.github.com/eric-czech/2784e9ac50745a5d2915

library(foreach)    # install.packages('foreach')
library(caret)      # install.packages('caret', dependencies = c("Depends", "Suggests"))
library(doParallel) # install.packages('doParallel')
registerDoParallel(makeCluster(4)) # Use 4 cores for parallel CV
###major parallelism implementations in R
# multicore, snow, parallel, Rmpi
# the "foreach" package allows for one interface to each of these.
#  (this particular code uses parallel)
# You can directly use, say snowfall, by doing:
# library(snowfall)
# sfInit(parallel=TRUE, cpus=4)
# listForApply "=" list with the K vectors of training or validation indices
# sfClusterApplyLB(listForApply,cvF,additionalArgs)

data <- read.csv(…) # Assuming this is your dataset 

cv <- caret::createFolds(nrow(data), k=10, list=T) # Create 10 folds
#K = 10
#kFold          = sample(1:n)
#validationSets = vector('list',K)
#validationSize = n/K
#for(k in 1:K){
#  validationSets[[k]] = kFold[((k-1)*(validationSize)+1):(k*validationSize)]  
#}

# 'dopar' here would run this on multiple threads (change to just 'do' for synchronous runs)
results <- foreach(fold = cv) %dopar% {
  
  # Get the fold data where 'fold' is nothing more than a list of indexes for test observations in the data
  data.train <- data[-fold] # Get the opposite of the test observations to train on
  data.test <- data[fold]
  
  # Fit the model and make predictions
  fit <- glm(response ~ var1 + var2, data=data.train, family='binomial')
  y.pred <- predict(fit, newdata=data.test)
  y.true <- data.test$response
  
  # Return 2x2 table of predictions vs actual values as well as the fit model (so you could check coefficients)
  list(accuracy=table(y.pred, y.true, dnn=c('predicted', 'actual')),  model=fit)
}

# Results is a list, so you could process it to extract the accuracies like this:

library(dplyr) # install.packages('dplyr')
foreach(fold.result=results, fold.num=icount(), .combine=rbind) %do%{
  as.data.frame(fold.result$accuracy) %>% 
    mutate(actual = as.character(actual), predicted = as.character(predicted)) %>%
    group_by(Correct=actual == predicted) %>%  # Determine whether or not the predictions were correct
    summarise(Count=sum(Freq)) %>%  # Count the number of such predictions
    mutate(Fold=fold.num)  # Add the fold number to this result
}