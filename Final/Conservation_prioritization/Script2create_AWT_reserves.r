################################################################################
#Script to generate databases of climate data aggregated to river basins

library(SDMTools)
source('/home/jc148322/scripts/libraries/cool_functions.r')

### Find out which SegmentNos are in the Wet Tropics region

raster=read.asc('/home/jc248851/ZONATION/CASSIE/RF/SegmentNo_AWT.asc') #read in contintental aust 250m segNo raster
base.asc=read.asc('/home/jc246980/Zonation/mask.asc')
reserve=read.asc('/home/jc246980/Zonation/natres250.asc')

pos=make.pos(base.asc)
pos$mask=extract.data(cbind(pos$lon, pos$lat),base.asc)
pos$reserves=extract.data(cbind(pos$lon, pos$lat),reserve)
pos$SegmentNo=extract.data(cbind(pos$lon, pos$lat),raster) #extract the SegmentNos for the WT
pos$SegmentNo[which(is.na(pos$mask))]=NA

out.dir ="/home/jc246980/Zonation/"
setwd(out.dir)

tasc=make.asc(pos[,'reserves'])		
write.asc.gz(tasc, filename)


