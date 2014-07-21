###Script to locate internally draining river networks and associated catchments

library(SDMTools); library(maptools)#load the necessary library

load('/home/jc148322/NARPfreshwater/SDM/connectivity.file.Rdata') #load position data of segmentNo and connected 'sub-graphs
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   
Terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')	
D2OUTLET=Terrain[,c('SEGMENTNO', 'D2OUTLET')]

D2OUTLET[which(D2OUTLET$SEGMENTNO == 749834),]

CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))    # read in the catchment ascii grid file at 250m resolution
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]    

wd = "/home/jc246980/Janet_Stein_data/"; setwd(wd)   
Distance.asc = read.asc(paste(wd,'d2coast.asc', sep='')) 

pos250$D2Coast = Distance.asc[cbind(pos250$row,pos250$col)]    

Distance_averaged = aggregate(pos250$D2Coast, by = list(pos250$SegmentNo), min)  
colnames(Distance_averaged)=c("SEGMENTNO", "D2Coast")

Tdata=merge(Distance_averaged, D2OUTLET, by=c('SEGMENTNO'))

Mouths = Tdata[which(Tdata$D2OUTLET<=1),] # Identifies all end of river mouth segments
connectivity=as.data.frame(connectivity)
colnames(connectivity)=c("SEGMENTNO", "catchments")

Mouths=merge(Mouths, connectivity, by=c('SEGMENTNO'))

out_Drains = Mouths[which(Mouths$D2Coast <1),c('catchments')] # Identifies all end of river mouth segments that are coastal draining

in_Drains=Mouths[which(!(Mouths[,'catchments'] %in% out_Drains)),c('catchments')] #removals coastal catchments from catchment list


internal_draining_segments=connectivity[which(connectivity$catchments %in% in_Drains),]

write.csv(internal_draining_segments,paste("/home/jc246980/SDM/internal_draining_segments.csv", sep = ''), row.names = F )
