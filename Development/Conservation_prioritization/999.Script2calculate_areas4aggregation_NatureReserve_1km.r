################################################################################
#Script to generate databases of climate data aggregated to river basins

library(SDMTools) #load the necessary library
library(maptools)


wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                          
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))   
tasc=CatchmentRaster.asc 
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 

# Set up unique identifier for 250 km resolution grid
pos250$UID = 1:111312265                                                        # add unique identifier to each 1km grid cell
tasc[cbind(pos250[,'row'],pos250[,'col'])] =  pos250[,'UID']                             # append unique identifier to asc using pos dataframe with unique ID



NRasc = read.asc("/home/jc246980/Zonation/natres250.asc") 					 # load nature reserve asc
pos250 = as.data.frame(which(is.finite(NRasc),arr.ind=T))        			 # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(NRasc)$y[pos250$col]                     			 # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(NRasc)$x[pos250$row] 
pos250$natres = NRasc[cbind(pos250$row,pos250$col)]              			 # append nature reserve code
cellareas.asc <- grid.area(NRasc)[,]                                         # calculate areas of 250m grids as they vary slightly in area with the curvature of the earth
pos250$Area250 = cellareas.asc[cbind(pos250$row,pos250$col)]                 # append 250 area data to position file
pos250$Km250UID  = extract.data(cbind(pos250$lon,pos250$lat), tasc)          # map 5 km UID onto 250 m NR   - 250 m reaches sometimes cross >1 5km grid square



Area_agg = aggregate(pos250$Area250, by = list(pos250$ramsar, pos250$OneKmUID), sum)  #aggregate area by unqiue combination of 5 km UID and Reach ID.
colnames(Area_agg) =c('ramsar', 'UID','AREA')

# script to create the weights

Ramsar_area = aggregate(Area_agg$AREA, by = list(Area_agg$ramsar), sum)    # calculate total ramsar area   
colnames(Ramsar_area)=c('ramsar', 'Ramsar_area')
Ramsar_area_agg<- merge(Area_agg, Ramsar_area, by='ramsar')  
Ramsar_area_agg$weights =Ramsar_area_agg$AREA/Ramsar_area_agg$Ramsar_area 
wd='/home/jc246980/RAMSAR/'                
save(Ramsar_area_agg,file=paste(wd,'Area_aggregated_by_ramsar_1km.Rdata',sep=''))           # Save files out



