################################################################################
#Script to generate databases of climate data aggregated to river basins

library(SDMTools) #load the necessary library


wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)     # define and set working directory
baseasc = read.asc.gz('base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
tascQrun=baseasc                                                                # Rename baseasc for appending runoff data
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution

# Set up unique identifier for 5 km resolution grid

pos$UID = 1:286244                                                            # add unique identifier to each 5km grid cell
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
pos250$FiveKmUID  = extract.data(cbind(pos250$lon,pos250$lat), tasc)            #map 5 km UID onto 250 m Reach ID's   - 250 m reaches sometimes cross >1 5km grid square


Area_agg = aggregate(pos250$Area250, by = list(pos250$REACHID, pos250$FiveKmUID), sum)  #aggregate area by unqiue combination of 5 km UID and Reach ID.
colnames(Area_agg) =c('REACHID', 'UID','AREA')
                     
save(Area_agg,file=paste(wd,'Area_aggregated_by_UID_5km.Rdata',sep=''))           # Save files out

################################################################################
# aggregate runoff generated at 5km resolution onto Janets reaches

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244 

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   

load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") 


# create means for 30 years

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun) = tt #add the column names

Q_run_curmean_dynamo=NULL
Q_run_cursd_dynamo=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
           Q_run_cursd_dynamo=cbind(Q_run_cursd_dynamo,tdata_cursd)
      }


# calculate annual total runoff per km2 for now

pos$runoff_rate =  rowSums(Q_run_curmean_dynamo)         

FINAL<- merge(Area_agg, pos, by='UID')                                          # Merge Area_agg with 5km pos file                            

FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                         # Multiply area(in km) by runoff (in mm/km)
 
Reach_runoff = aggregate(FINAL$Runoff, by = list(FINAL$REACHID), sum) 

colnames(Reach_runoff) =c('REACHID', 'Runoff')

save(Reach_runoff,file=paste(wd,'Reach_runoff_5km.Rdata',sep=''))     


