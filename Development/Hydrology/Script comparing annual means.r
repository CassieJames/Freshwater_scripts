qsub -l nodes=2 -l pmem=5gb -I
library(SDMTools) #load the necessary library
load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") # load area relationships for aggregating

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244 



#################################################################################################################################
### First trial - aggregate runoff at monthly basis before determining annual sum
Qrun_agg=Qrun

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun_agg) = tt #add the column names

### Calculate monthly means for thirty years

Q_run_curmean_dynamo=NULL
    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

	   tdata_curmean = rowMeans(Qrun_agg[,which(as.numeric(substr(colnames(Qrun_agg),1,2))==mm)],na.rm=TRUE) #calculate row means
	   Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
    }

mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
tt = expand.grid(mm,"Cur_dyn");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Q_run_curmean_dynamo) = tt #add the column names

###Aggregate monthly means to reaches

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

runoff=as.data.frame(Runoff)
runoff$annual=rowSums(runoff[,1:12]) # calculate annual means

#################################################################################################################################
### second trial - determine annual runoff on 5km grid basis before aggregating to reaches

Q_run_curmean_dynamo=as.data.frame(Q_run_curmean_dynamo)
Q_run_curmean_dynamo$annual=rowSums(Q_run_curmean_dynamo[,1:12])
tdata=pos 																		  # make a copy of pos with UID appended
tdata$runoff=Q_run_curmean_dynamo$annual
FINAL<- merge(Area_agg, tdata, by='UID')    
FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                           # Multiply area(in km) by runoff (in mm/km)
Reach_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), sum) 
