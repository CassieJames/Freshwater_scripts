################################################################################
# aggregate corrected runoff generated at 5km resolution onto Janets reaches for 1961 to 1990 currents

library(SDMTools) #load the necessary library

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244 
out.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/"

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") 

future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085,10)

####Currents
# Load currents, calculate monthly means and aggregate to reach for dynamic script
load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic_1960.Rdata")  # Load runoff
Qrun_agg=Qrun

yois=1961:1990
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun_agg) = tt #add the column names

Q_run_curmean_dynamo=NULL

    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

	   tdata_curmean = rowMeans(Qrun_agg[,which(as.numeric(substr(colnames(Qrun_agg),1,2))==mm)],na.rm=TRUE) #calculate row means
	   Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
    }

mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

tt = expand.grid(mm,"Cur_dyn");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Q_run_curmean_dynamo) = tt #add the column names
Runoff=NULL
	for (ii in 1:ncol(Q_run_curmean_dynamo)){
		
		tdata=pos 																		  # make a copy of pos with UID appended
		tdata$runoff_rate=Q_run_curmean_dynamo[,ii] 									  # Append gridded runoff data
		FINAL<- merge(Area_agg, tdata, by='UID')                                          # Merge Area_agg with 5km pos file                            
		FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                           # Multiply area(in km) by runoff (in mm/km)
		Reach_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), sum) 	  # Aggregate runoff to reaches
		Runoff=cbind(Runoff, Reach_runoff_final$x)

	}
tt = expand.grid(mm,"Cur_dyn");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Runoff) = tt #add the column names
save(Runoff, file=paste(out.dir,"Current_dynamic.Rdata",sep='')) #save the runoff	

# Load currents and aggregate to reach for static script

load("/home/jc246980/Hydrology.trials/Outputs/Output_1960_1990/Q_run_current_5km_means_1960.Rdata")  # Load runoff
Qrun_curmean=Qrun
Runoff=NULL
	for (ii in 1:ncol(Qrun_curmean)){
		
		tdata=pos 																		  # make a copy of pos with UID appended
		tdata$runoff_rate=Qrun_curmean[,ii] 										  # Append gridded runoff data
		FINAL<- merge(Area_agg, tdata, by='UID')                                          # Merge Area_agg with 5km pos file                            
		FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                           # Multiply area(in km) by runoff (in mm/km)
		Reach_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), sum) 	  # Aggregate runoff to reaches
		Runoff=cbind(Runoff, Reach_runoff_final$x)

	}
tt = expand.grid(mm,"Cur_stat");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Runoff) = tt #add the column names
save(Runoff, file=paste(out.dir,"Current_static.Rdata",sep='')) #save the runoff	
	
