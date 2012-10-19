################################################################################
#Script to generate databases of climate data aggregated to river basins

library(SDMTools) #load the necessary library


wd = '/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05'; setwd(wd)     # define and set working directory
baseasc = read.asc.gz('base.asc.gz')                                            # Import base asc at 1 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 1 km resolution for appending identifier
tascQrun=baseasc                                                                # Rename baseasc for appending runoff data
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 1 km resolution

# Set up unique identifier for 1 km resolution grid

pos$UID = 1:6978397                                                             # add unique identifier to each 1km grid cell
tasc[cbind(pos[,'row'],pos[,'col'])] =  pos[,'UID']                             # append unique identifier to asc using pos dataframe with unique ID
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                              # change wd for other inputs and  outputs

# Set up 250 m raster grid with reach identifier appended  and aggregate area by UID's

CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))    # read in the catchment ascii grid file at 250m resolution
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$REACHID = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]              #append ReachID (1:1474286)
cellareas.asc <- grid.area(CatchmentRaster.asc)[,]                              #calculate areas of 250m grids as they vary slightly in area with the curvature of the earth
pos250$Area250 = cellareas.asc[cbind(pos250$row,pos250$col)]                    #append 250 area data to position file
pos250$OneKmUID  = extract.data(cbind(pos250$lon,pos250$lat), tasc)             #map one km UID onto 250 m Reach ID's   - 250 m reaches sometimes cross >1 1km grid square


Area_agg = aggregate(pos250$Area250, by = list(pos250$REACHID, pos250$OneKmUID), sum)  #aggregate area by unqiue combination of one km UID and Reach ID.


                      # Save files out
save(Area_agg,file=paste(wd,'Area_aggregated_by_UID.Rdata',sep=''))

################################################################################
# aggregate runoff generated at 1km resolution onto Janets reaches

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   
library(SDMTools) #load the necessary library


load("/home/jc246980/Hydrology.trials/Outputs1km/Q_run_current1km.Rdata")       # Load Area_aggregated by unique combinations of reach ID and 1km id
#load(paste(wd,'Area_aggregated_by_UID.Rdata',sep='')) 
#load(paste(wd,"pos250", sep=''))                          

tdata = as.data.frame(rowSums(Q_run))                                           # calculate annual total runoff per km2 for now
tascQrun[cbind(pos[,'row'],pos[,'col'])] =  tdata[,1]                           # append annual runoff total to 1 km res ascii
pos250$Q_run_annual = extract.data(cbind(pos250$lon,pos250$lat), tascQrun)      # extract annual runoff data based on lon and lat positions of 250 m grid
pos250$Area250inKM = pos250$Area250/1000000                                     # convert m2 to km2 area
pos250$RunOff_K2 = pos250$Q_run_annual*pos250$Area250inKM                       # calculate runoff per unit area

#save(pos250,file=paste(wd,'pos250.Rdata',sep=''))   
#load(paste(wd, 'pos250.Rdata', sep=''))  

RunOff_Reach = aggregate(pos250$RunOff_K2, by = list(pos250$REACHID), sum)      # calculate runoff per reach
pos250$Q_run_annual = extract.data(cbind(pos250$lon,pos250$lat), tascQrun)      # Now need to extract this data back to the 250m raster!!
pos250trial=pos250
colnames(RunOff_Reach) = c('REACHID','REACH_RUNOFF')
tdata<- merge(pos250trial, RunOff_Reach, by.x = "REACHID", by.y = "REACHID")

#save(tdata,file=paste(wd,'ReachRunoff_Annual.Rdata',sep=''))   
#load(paste(wd,'ReachRunoff_Annual.Rdata',sep=''))


################################################################################
library(SDMTools) #load the necessary library

image.dir='/home/jc246980/Hydrology.trials/Images1km'
wd = '/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05'; setwd(wd)     # define and set working directory
base.asc = read.asc.gz('base.asc.gz')     
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))    
cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define points for legend
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                              
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep='')) 

png(paste(image.dir,'/RunOff_reach.png',sep=''), width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*1.5+60, units='px', pointsize=30, bg='lightgrey')
    
    zlim= c(0,13326.72); range(quantile(tdata[,'REACH_RUNOFF'],c(0.01,0.5,0.99)))
    tasc = CatchmentRaster.asc; tasc[cbind(pos250$row,pos250$col)]=tdata[,'REACH_RUNOFF']
    image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
    legend.gradient(pnts,cols=cols,limits=round(zlim), title="Runoff per stream segment", cex=3)
    
    dev.off()
