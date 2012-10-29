###################################################################################################
# Script to aggregate future runoff to reach (with the currents (dynamic and static) appended to the front of the table for later corrections

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
data.dir="/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/Qrun"
out.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/"
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085,10)
pos = read.csv('base.positions.csv',as.is=TRUE)# read in positions on grid at 5 km resolution
pos$UID = 1:286244

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/Current_dynamic.Rdata")  # Load runoff dynamic for current
Runoff_agg=Runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/Current_static.Rdata")  # Load runoff static for current
Runoff_stat=Runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") 


	for(es in ESs) { 
	
		for(gcm in GCMs) {

			load(paste(data.dir,"/",es,"_",gcm,".Rdata",sep=''))
			Runoff=matrix(NA, nrow=1466889, ncol=1)
			Runoff[,1]=1:1466889; colnames(Runoff)[1]=c("SegmentNo")
			Runoff=cbind(Runoff,Runoff_stat) # append current static data
			Runoff=cbind(Runoff,Runoff_agg)  # append current dynamic data
			for (ii in 1:ncol(Qrun)){
				
				tdata=pos 																		  # make a copy of pos with UID appended
				tdata$runoff_rate=Qrun[,ii] 										              # Append gridded runoff data
				FINAL<- merge(Area_agg, tdata, by='UID')                                          # Merge Area_agg with 5km pos file                            
				FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                           # Multiply area(in km) by runoff (in mm/km)
				Reach_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), sum) 	  # Aggregate runoff to reaches
				Runoff=cbind(Runoff, Reach_runoff_final$x)

			}
		tt = expand.grid(mm,YEARs=YEARs);tt = paste(tt[,2],tt[,1],sep='_'); colnames(Runoff)[26:121] = tt #add the column names
		save(Runoff, file=paste(out.dir,es,"_",gcm,".Rdata",sep='')) #save the runoff out	
		
		}
	}