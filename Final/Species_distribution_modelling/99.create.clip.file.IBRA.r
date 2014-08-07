### Quick script to recreate clip file C James May 12th 2014

library(SDMTools); library(maptools) #load the necessary library

catchrast=load("/home/jc246980/Obsolete/Hydrology.trials/Catchmentraster250.Rdata")
ibras = read.asc("/home/jc246980/SDM/ibra.asc")
riverbasins = read.asc("/home/jc246980/SDM/riverbasin.asc")

pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))        			 # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     			 # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]     
pos250$riverbasin = riverbasins[cbind(pos250$row,pos250$col)]      
pos250$ibra = extract.data(cbind(pos250$lon,pos250$lat), ibras)     
tdata=pos250[,c("SegmentNo", "riverbasin", "ibra")]


clipibra=tdata[!duplicated(tdata[,1]), ]
colnames(clipibra)=c("SegmentNo", "Clip2RB", "Clip2IBRA")
save(clipibra,file='/home/jc246980/SDM/clipIBRA.Rdata') #load position data of segmentNo and regions. object called clip.
