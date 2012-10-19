################################################################################
#Script to accumulate flow

library(SDMTools) #load the necessary library
library(maptools)

network=read.dbf("/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf")
load("/home/jc246980/Hydrology.trials/Reach_runoff_5km.Rdata")
voi <- c('OBJECTID', 'HydroID', 'SegmentNo', 'NextDownID')
networktemp <- network[voi]

colnames(Reach_runoff) =c('SegmentNo', 'Runoff')
tempdat<- merge(networktemp, Reach_runoff, by='SegmentNo')

HydroID=tempdat$HydroID
NextDownID=tempdat$NextDownID
Runoff=tempdat$Runoff
tempdat$Runoffacc=NULL

Accum.function = function(HydroID,NextDownID,Runoff) {
    headterminus.HydroID <- as.matrix(HydroID[!(HydroID %in% NextDownID)])    # hydroIDs for head terminus
    if (!headterminus.HydroID %in% NextDownID) { Runoffacc = Runoff
        else {
        n=2
        #Order[,paste(n)] = NextDownID[headterminus.HydroID %in% HydroID]
        #runoffacc = Runoff[(Order[,paste(n)]%in% HydroID)]+Runoff[(Order[,paste(n)]%in% NextDownID)]


        }
    }