################################################################################
#Script to generate databases of climate data aggregated to river basins

library(SDMTools) #load the necessary library
library(maptools)

raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz')
tasc=raster    
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon

# Set up unique identifier for 1 km resolution grid
pos$UID = 1:6957044                                                          	# add unique identifier to each 5km grid cell
tasc[cbind(pos[,'row'],pos[,'col'])] =  pos[,'UID']                             # append unique identifier to asc using pos dataframe with unique ID
                       # change wd for other inputs and  outputs
# Set up 250 m raster grid with reach identifier appended  and aggregate area by UID's


RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_final.asc") # load ramsar  asc # load ramsar  asc
pos250 = as.data.frame(which(is.finite(RAMasc),arr.ind=T))        			 # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(RAMasc)$y[pos250$col]                     			 # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(RAMasc)$x[pos250$row] 
pos250$ramsar = RAMasc[cbind(pos250$row,pos250$col)]              		 # append Riverbasin numbers (1:245)
cellareas.asc <- grid.area(RAMasc)[,]                                         # calculate areas of 250m grids as they vary slightly in area with the curvature of the earth
pos250$Area250 = cellareas.asc[cbind(pos250$row,pos250$col)]                     # append 250 area data to position file
pos250$OneKmUID  = extract.data(cbind(pos250$lon,pos250$lat), tasc)             # map 5 km UID onto 250 m Riverbasin ID's   - 250 m reaches sometimes cross >1 5km grid square



Area_agg = aggregate(pos250$Area250, by = list(pos250$ramsar, pos250$OneKmUID), sum)  #aggregate area by unqiue combination of 5 km UID and Reach ID.
colnames(Area_agg) =c('ramsar', 'UID','AREA')

# script to create the weights

Ramsar_area = aggregate(Area_agg$AREA, by = list(Area_agg$ramsar), sum)    # calculate total ramsar area   
colnames(Ramsar_area)=c('ramsar', 'Ramsar_area')
Ramsar_area_agg<- merge(Area_agg, Ramsar_area, by='ramsar')  
Ramsar_area_agg$weights =Ramsar_area_agg$AREA/Ramsar_area_agg$Ramsar_area 
wd='/home/jc246980/RAMSAR/'                
save(Ramsar_area_agg,file=paste(wd,'Area_aggregated_by_ramsar_1km_all.Rdata',sep=''))           # Save files out



