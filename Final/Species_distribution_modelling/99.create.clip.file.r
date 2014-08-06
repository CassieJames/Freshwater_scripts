### Quick script to recreate clip file C James May 12th 2014

library(SDMTools); library(maptools) #load the necessary library

catchrast=load("/home/jc246980/Obsolete/Hydrology.trials/Catchmentraster250.Rdata")
bioregions = read.asc("/home/jc246980/SDM/fishbio.asc")
riverbasins = read.asc("/home/jc246980/SDM/riverbasin.asc")

pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))        			 # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     			 # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]     
pos250$riverbasin = riverbasins[cbind(pos250$row,pos250$col)]      
pos250$bioregion = extract.data(cbind(pos250$lon,pos250$lat), bioregions)     
tdata=pos250[,c("SegmentNo", "riverbasin", "bioregion")]


clipnew=tdata[!duplicated(tdata[,1]), ]
colnames(clipnew)=c("SegmentNo", "Clip2RB", "Clip2Bio")
save(clipnew,file='/home/jc246980/SDM/clipnew.Rdata') #load position data of segmentNo and regions. object called clip.
