### Script to clip distributions where necessary to IBRA or state

library(SDMTools) #load the necessary library


wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                              # change wd for other inputs and  outputs

# Set up 250 m raster grid with reach identifier appended  and aggregate area by UID's

CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))    # read in the catchment ascii grid file at 250m resolution
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))  

pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]   
pos250$IBRA = extract.data(cbind(pos250$lon,pos250$lat), IBRA.asc)   

IBRA.asc = read.asc('/home/jc246980/SDM/ibra_250m.asc')    # read in IBRA asc
pos250$IBRA = extract.data(cbind(pos250$lon,pos250$lat), IBRA.asc)     

state.asc = read.asc('/home/jc246980/SDM/states.asc')    # read in IBRA asc
pos250$STATE = extract.data(cbind(pos250$lon,pos250$lat), state.asc)    

NSW<-pos250[which(pos250$STATE==1),]
VIC<-pos250[which(pos250$STATE==2),]
QLD<-pos250[which(pos250$STATE==3),]
SA<-pos250[which(pos250$STATE==4),]
WA<-pos250[which(pos250$STATE==5),]
TAS<-pos250[which(pos250$STATE==6),]
NT<-pos250[which(pos250$STATE==7),]
ACT<-pos250[which(pos250$STATE==8),]
 