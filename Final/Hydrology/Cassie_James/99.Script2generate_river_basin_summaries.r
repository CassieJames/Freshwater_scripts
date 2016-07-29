###################################################################################################
### Code to compare accumulated and absolute runoff for river basins across Australia

library(SDMTools) #define the libraries needed

##########set up current climate baseline and attribute identifier
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244 																# append unique identifier

##########import river basin asc and attribute river basin number to position table

river.basin.dir='/home/jc246980/Obsolete/Hydrology.trials/Accumulated_reach/Flow_accumulation_background_stuff/';  setwd(river.basin.dir)
tasc = read.asc("bname.asc")
pos$riverbasin  = extract.data(cbind(pos$lon,pos$lat), tasc)  
RB=unique(pos$riverbasin)

##########import area relationships

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_riverbasin_5km.Rdata') #load area relationships table

##########Load raw runoff data (not accumulated!)

load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg.Rdata")  # Load runoff
Qrun_agg=Q_run

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun_agg) = tt #add the column names

Q_run_curmean_dynamo=NULL

    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

	   tdata_curmean = rowMeans(Qrun_agg[,which(as.numeric(substr(colnames(Qrun_agg),1,2))==mm)],na.rm=TRUE) #calculate row means
	   Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
    }

mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
tt = expand.grid(mm,"Cur_dyn");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Q_run_curmean_dynamo) = tt #add the column names

Q_run_curmean_dynamo=as.data.frame(Q_run_curmean_dynamo) # Monthly data

##########Load raw runoff data mean values
load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg.Rdata")  # Load runoff


tdata=pos 																		  # make a copy of pos with UID appended
tdata$annual=rowSums(Q_run_curmean_dynamo)										              # Append gridded runoff data
FINAL<- merge(Area_agg, tdata, by='UID')                                          # Merge Area_agg with 5km pos file                            
FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$annual
riverbasin_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$riverbasin), sum) 


wd="/home/jc246980/Hydrology.trials/"
write.csv(riverbasin_runoff_final,paste(wd,'Runoff_summed_across_riverbasins_agg_static.csv',sep=''))



















