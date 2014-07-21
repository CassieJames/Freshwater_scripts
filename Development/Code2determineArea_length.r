
library(SDMTools); library(maptools); library(plyr) #load the necessary library

                     
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')	
Catchatts = read.dbf('/home/jc246980/Janet_Stein_data/AHGFCatchments.dbf')	
Terrainatts = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')	

length=networkatts[,c('SegmentNo', 'GeodesLen')]
area=Catchatts[,c('SegmentNo', 'AlbersArea')]

image.dir = "/home/jc246980/Publications/Frog_paper/" ; setwd(image.dir)  

tdata=merge(length, area, by=c('SegmentNo'))


base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file

png('Segment length to catchment area catch logged+1.png')

plot(log((tdata$GeodesLen/1000)+1), log((tdata$AlbersArea/1000000)+1), main="Stream length versus catchment area",
   xlab="Stream length (km) (log+1) ", ylab="Catchment area (km2) (log+1) ", pch=19) 
dev.off()

