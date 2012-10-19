################################################################################
# aggregate bioclim variables generated at 5km resolution onto Janets reaches

library(SDMTools) #load the necessary library


wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)     # define and set working directory
baseasc = read.asc.gz('base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
tascQrun=baseasc                                                                # Rename baseasc for appending runoff data
pos = read.csv('base.positions.csv',as.is=TRUE)      

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   

# Load Area_aggregated by unique combinations of reach ID and 1km id
load(paste(wd,'Area_aggregated_by_UID.Rdata',sep='')) 
                       

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
