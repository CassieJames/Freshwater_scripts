###################################################################################################
#### Script to prepare future annual hydrology data for water hole persistence modelling
#### C. James...............9th January 2012

# Load in necessary libraries
library(SDMTools)

# Set up directories
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
accum.dir = "/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/"
runoff.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/"
out.dir ="/home/jc246980/Hydrology.trials/Hydrology metrics/Metrics_future_1960base/"

wd ="/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1960to1990/"

current_dynamic=read.csv(paste(wd, "Current_dynamic.csv", sep=''))
current_static=read.csv(paste(wd, "Current_static.csv", sep=''))
currents=merge(current_static, current_dynamic,by="SegmentNo")

ESs = list.files(futdir, pattern="RCP")[4] 										# list the emission scenarios
GCMs = c('inm-cm30', 'mri-cgcm232a', 'csiro-mk30', 'ncar-pcm1', 'ccsr-miroc32med', 'ipsl-cm4', 'mpi-echam5', 'cccma-cgcm31', 'iap-fgoals10g')# get a list of GCMs of interest
YEAR=c('2035', '2055', '2085')

	for (es in ESs) {
	
		for (gcm in GCMs) {
			
			load(paste(runoff.dir,es,"_",gcm,".Rdata",sep='')) #loads runoff data

			for (yy in YEAR) {
			
				cois= c(1,grep(yy,names(Runoff)))#columns of interest plus SegmentNo
				runoff.data= Runoff[,cois]
				runoff.data$Annual_mean =rowSums(runoff.data[,c(2:13)]) # determine annual mean runoff
				
				accum.data= read.csv(paste(accum.dir,es,"_",gcm,".",yy,".csv",sep='')) #loads acummulated data
				tdata=merge(currents, accum.data, by="SegmentNo")
				tdata$Annual_mean=rowSums((tdata[,c(14:25)]+1)*((tdata[,c(26:37)])/(tdata[,c(2:13)]+1))) # apply correction
				
				Accum_flow=tdata[,c("SegmentNo", "Annual_mean")]
				Runoff_local=runoff.data[,c("SegmentNo", "Annual_mean")]
				Hydro_missing=Runoff_local[which(!(Runoff_local$SegmentNo %in% Accum_flow$SegmentNo)),]   
				ANNUAL_MEAN=rbind(Accum_flow, Hydro_missing)
				
				write.csv(ANNUAL_MEAN,paste(out.dir,es,"_",gcm,"_",yy,".csv",sep=''),row.names=F)	
			}
		}
	}

