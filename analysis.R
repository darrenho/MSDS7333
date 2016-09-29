source('indoorPositioningF.R')
errorCheck = 0
offline_out = readDataF(readLines('../Data/offline.final.trace.txt'),
                        errorCheck=errorCheck)
                        
online_out  = readDataF(readLines('../Data/online.final.trace.txt'),
                        errorCheck=errorCheck)

require(ggplot2)
pdf('offlineLocations.pdf')
offline_out$locCounts %>%
  ggplot() + geom_point(aes(posX,posY))
dev.off()
cat('made a plot in directory: ',getwd(),'\n')


require(lattice)
bwplot(signal ~ factor(angle) | mac, data=offline_out$dataSet,
       subset = posX == 2 & posY == 12 & mac != "00:0f:a3:39:dd:cd",
       layout = c(2,3))

offline = offline_out$dataSet

offline$posXY = paste(offline$posX,offline$posY,sep='-')

ans = offline %>% 
  group_by(posXY,angle,mac) %>%
  summarise(medianSignal = median(signal),
            meanSignal = mean(signal),
            count      = n(),
            sdSignal   = sd(signal),
            iqrSignal  = IQR(signal))

breaks = seq(-90,-30,by=5)
bwplot(sdSignal ~ cut(meanSignal,breaks=breaks), 
       data=ans,
       subset = mac != "00:0f:a3:39:dd:cd")

AP = matrix(c(7.5,6.3,2.5,-.8,12.8,-2.8,1,14,33.5,9.3,33.5,2.8),
            ncol=2,byrow=TRUE,dimnames=list(offline_out$subMacs[-2],c("x","y")))

offlineSummary = filter(offline_out$dataSet,mac!=offline_out$subMacs[2])

offlineSummary = offlineSummary %>%
  group_by(mac) %>%
  mutate(dist = sqrt((posX - AP[mac,1])^2 + (posY - AP[mac,1])^2) )

pdf('signalByDistance.pdf')
xyplot(signal~dist | factor(mac)  + factor(angle),
       data=offlineSummary,pch=19,cex=0.3,xlab='distance')
dev.off()

# Online analysis
online_out
online = online_out$dataSet

online$posXY = paste(online$posX,online$posY,sep='-')
