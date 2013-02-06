###################################################################################################
### Script to setup current files for MAXENT run trials

library(SDMTools) #define the libraries needed

### set up input data
hydro=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv") # read in accumulated flow
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata") # read in runoff
bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv") # read in bioclim variables
dryseason.dir="/home/jc246980/DrySeason/DrySeason_reach/"
VOIS=c("num.month", "total.severity", "max.clust.length","clust.severity", "month.max.clust")

### Create annual sums and fill in accumulated flow gaps

hydro$SumAnnual=rowSums(hydro[,2:13])
Runoff$SumAnnual=rowSums(Runoff[,2:13])
hydro_extra=Runoff[which(!(Runoff$SegmentNo %in% hydro$SegmentNo)),]   
HYDRO=hydro[,c(1,14)]
HYDRO_EXTRA=hydro_extra[, c(1,14)]
HYDROLOGY=rbind(HYDRO,HYDRO_EXTRA)

### Create current environmental data file

	for(voi in VOIS) { cat(voi,'\n') 	
			
		tdata=read.csv(paste(dryseason.dir,"Current_",voi,".csv", sep='')) 			# load data for each varable
			
		if(voi=="num.month"){
		Enviro_dat=merge(bioclim,tdata, by="SegmentNo", all.x=TRUE)
		}else{Enviro_dat=merge(Enviro_dat,tdata, by="SegmentNo", all.x=TRUE)}
	
	}

Enviro_dat=merge(Enviro_dat, HYDROLOGY, by="SegmentNo", all.x=TRUE)

out.dir ="/home/jc246980/SDM/Environmental_future/"
write.csv(Enviro_dat,paste(out.dir,"Current_enviro_data.csv",sep=''),row.names=F)