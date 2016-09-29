readDataF = function(txt,errorCheck=2){
  ### 
  # I used "dataSet" here instead of offline as we will use online too
  # I tried to use triple "#" to indicate code from book that is replaced
  ###
  require(dplyr) 
  if(errorCheck > 0){ options(error = recover, warn=2)}
  #process the mac addresses
  breakSemiColonEqualsComma = strsplit(txt[4],"[;=,]")[[1]]

  selectSignalsDevice      = c(2,4,6:8,10)
  selectSignalsObservation = -(1:10)
  tmp = matrix(breakSemiColonEqualsComma[selectSignalsObservation], 
               ncol = 4, byrow = TRUE)
  mat = cbind(matrix(breakSemiColonEqualsComma[selectSignalsDevice],
                     nrow=nrow(tmp),ncol=6,byrow=TRUE),
              tmp)
  
  processLineF = function(txtItem){
    #This is a definite candidate for making a separate file
    breakSemiColonEqualsComma = strsplit(txtItem,"[;=,]")[[1]]
    if(length(breakSemiColonEqualsComma)== 10){
      return(NULL)
    }
    tmp = matrix(breakSemiColonEqualsComma[selectSignalsObservation], 
                 ncol = 4, byrow = TRUE)
    mat = cbind(matrix(breakSemiColonEqualsComma[selectSignalsDevice],
                       nrow=nrow(tmp),ncol=6,byrow=TRUE),
                tmp)
    
  }

  if(errorCheck >= 2){
    # Small subset for testing
    tmp = lapply(txt[4:200],processLineF)
    # set options for error checking
    dataSet = as.data.frame(do.call("rbind",tmp))
  }else{
    #full training set
    lines = txt[substr(txt,1,1) != "#"]
    start = proc.time()[3]
    tmp   = lapply(lines,processLineF)
    dataSet = as.data.frame(do.call("rbind",tmp),stringsAsFactors = FALSE)
    stop = proc.time()[3]
    cat('total time = ',stop - start,'\n')#24s
  }
  

  names(dataSet) = c("time","scanMac","posX","posY","posZ",
                     "orientation","mac","signal","channel",'type')
  
  ### offline = offline[ offline$type == "3", ]
  ### offline = offline[ , "type" != names(offline) ]
  dataSet = dataSet %>% 
    filter(type=="3") %>% 
    select(-type)
  
  numVars = c("time","posX","posY","posZ","orientation","signal")
  dataSet[numVars] = lapply(dataSet[numVars],as.numeric)
  
  ###offline$time = offline$time/1000
  dataSet = dataSet %>% 
    mutate(time = time/1000)
  # This overwrites time, what is an easy way keep original time as well?
  
  class(dataSet$time) = c("POSIXt","POSIXct")

  #only 1 detector on only X/Y plane: remove 
  ### code middle of pg 14.
  dataSet = dataSet %>% 
    select(-scanMac,-posZ)
  
  #group angles into nearest pi/4 and account for torus
  roundOrientation = function(angles){
    refs = seq(0,by=45,length=9)
    q    = sapply(angles,function(o) which.min(abs(o-refs)))
    return(c(refs[1:8],0)[q])
  }
  
  dataSet$angle = roundOrientation(dataSet$orientation)

  #get rid of extraneous Macs and remove channel as it is redundant with mac  
  ### subMacs = names(sort(table(dataSet$mac),decreasing=TRUE))[1:7]
  subMacs = names(table(dataSet$mac))[table(dataSet$mac) > 100000]
  dataSet = dataSet %>% 
    filter(dataSet$mac %in% subMacs) %>% 
    select(-channel)
  
  #Get total number of obs at each (X,Y)
  ## Note that dplyr helps mightily to simplify 1.3.3
  ### locDF = with(dataSet,by(dataSet,list(posX,posY),function(x)x))
  ### locDF = locDF[ !sapply(locDF,is.null) ]
  ### locCounts = sapply(locDF,nrow)
  locCounts = dataSet %>%
    group_by(posX,posY) %>%
    summarise(count = n())
  
  return(list('locCounts'=locCounts,'dataSet'=dataSet,'subMacs'=subMacs))
  #reset options:
  if(errorCheck > 0){ options(error = NULL)}
}