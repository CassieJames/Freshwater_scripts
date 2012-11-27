###################################################################################################
# Script to aggregate future runoff to reach (with the currents (dynamic and static) appended to the front of the table for later corrections
# C James November 2012

###Set directories
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
data.dir="/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/Qrun"
out.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/"
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085,10)
mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

###Set up inputs
pos = read.csv('base.positions.csv',as.is=TRUE)# read in positions on grid at 5 km resolution
pos$UID = 1:286244
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata") # Load runoff dynamic for current
Runoff_agg=Runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_static.Rdata") # Load runoff static for current
Runoff_stat=Runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata")

####Create column headings
ttstatic = expand.grid(mm,"Cur_stat");ttstatic = paste(ttstatic[,2],ttstatic[,1],sep='_')
ttdyn = expand.grid(mm,"Cur_dyn");ttdyn = paste(ttdyn[,2],ttdyn[,1],sep='_')
ttdat = expand.grid(mm,YEARs=YEARs);ttdat = paste(ttdat[,2],ttdat[,1],sep='_')
tt=c('SegmentNo',ttstatic,ttdyn,ttdat)

###Run loop to aggergate runoff to reach for each twelve month period

	for(es in ESs) {

			for(gcm in GCMs) {

			load(paste(data.dir,"/",es,"_",gcm,".Rdata",sep=''))
			Runoff=merge(Runoff_stat, Runoff_agg, by="SegmentNo")

					for (ii in 1:ncol(Qrun)){

							tdata=pos # make a copy of pos with UID appended
							tdata$runoff_rate=Qrun[,ii] # Append gridded runoff data
							FINAL<- merge(Area_agg, tdata, by='UID') # Merge Area_agg with 5km pos file
							FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate # Multiply area(in km) by runoff (in mm/km)
							Reach_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), sum) # Aggregate runoff to reaches
									
									if(ii==1){
									Runoff_dat=Reach_runoff_final
									}else{Runoff_dat=cbind(Runoff_dat, Reach_runoff_final$x)}
									Runoff_dat=round(Runoff_dat,4)
					}

			Runoff=merge(Runoff, Runoff_dat, by="SegmentNo")
			colnames(Runoff) = tt #add the column names
			save(Runoff, file=paste(out.dir,es,"_",gcm,".Rdata",sep='')) #save the runoff out

			}
	}