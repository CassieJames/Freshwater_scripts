###################################################################################################
# Script to prepare data for Zonation 
### C James 

library(SDMTools) #define the libraries needed
library(raster)

#### Set up 250 m raster grid with river segment identifier appended  
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                          
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))   
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]     

####  Setting up IBRAs, extract for correct locations and appending to dataset

ibra.dir='/home/jc246980/IBRA regions/';  setwd(ibra.dir)
tasc = read.asc("ibra_250m.asc")
pos250$IBRA = extract.data(cbind(pos250$lon,pos250$lat), tasc)  # extract data from tasc at pos250 lon and lat positions

####   Read in the river basins, extract for correct locations and append

tasc = read.asc("/home/jc246980/Zonation/river2.asc")
pos250$riverbasin  = extract.data(cbind(pos250$lon,pos250$lat), tasc)  
tdata=pos250[which(pos250$IBRA==88),]
roi = unique(na.omit(tdata$riverbasin)) # create a vector of all the riverbasins that transect IBRA 88 (AWT)
roi=roi[-c(1,5,10,13,14)]# remove rivers that are not encompassed by AWT region (these are positions in the vector not riverbasin codes- beware!)

#### Read in species data

species.data = read.csv("/home/jc246980/Species_data/Reach_data/Fish_reach.csv")
pos250= merge(pos250, species.data, by='SegmentNo')    


#### Clip data to min and max row and column numbers

tdata=pos250[which(pos250$riverbasin %in% roi),]

tdata=pos250[which(pos250$IBRA == 88),] # subset by IBRA for the time being
minrow=min(tdata$row)-1; maxrow=max(tdata$row)+1# work out the max and min row numbers for clipping
mincol=min(tdata$col)-1; maxcol=max(tdata$col)+1

tdata2=pos250[which(pos250$row<=maxrow & pos250$row>=minrow),] # clip data to row values between (and including min and max)
tdata3=tdata2[which(tdata2$col<=maxcol & tdata2$col>=mincol),] 

tdata3$IBRA[which(tdata3$row == minrow)] = -999
tdata3$IBRA[which(tdata3$row == maxrow)] = -999
tdata3$IBRA[which(tdata3$col == mincol)] = -999
tdata3$IBRA[which(tdata3$col == maxcol)] = -999

tdata3$SegmentNo[which(tdata3$row == minrow)] = -999
tdata3$SegmentNo[which(tdata3$row == maxrow)] = -999
tdata3$SegmentNo[which(tdata3$col == mincol)] = -999
tdata3$SegmentNo[which(tdata3$col == maxcol)] = -999

#Create a background ascii - doesn't work as its slightly misaligned!
minlat=min(tdata$lat); minlon=min(tdata$lon)
bg.asc= as.asc(matrix(rep(-999),722,1513), xll = minlon, yll =minlat , cellsize = 0.0025)
write.asc(bg.asc, "background.asc")


out.dir="/home/jc246980/Zonation/Species_ascs/"; setwd(out.dir)
tdata4 = tdata3[,c("lat", "lon", "IBRA", "SegmentNo")]
dataframe2asc(tdata4)





