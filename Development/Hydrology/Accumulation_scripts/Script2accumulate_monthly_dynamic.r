# Script to generate four annual runoff measures - dynamic and static for aggregation at monthly and at yearly

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244 
out.dir="/home/jc246980/Hydrology.trials/"

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") #Area_agg file

###################################################################################################
###Load raw data for dynamic
load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
Qrun_agg=Qrun

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


###Aggregate monthly data for dynamic model

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



out.dir="/home/jc246980/Hydrology.trials/Flow_accumulation/"

write.csv(Runoff,paste(out.dir,'runoff4accumulations_monthly_dynamic.csv',sep=''),row.names=F)

stream.data$SegmentNo=c(1:1:1466889)

stream.data=stream.data[,c(13,1,2,3,4,5,6,7,8,9,10,11,12)]

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata")