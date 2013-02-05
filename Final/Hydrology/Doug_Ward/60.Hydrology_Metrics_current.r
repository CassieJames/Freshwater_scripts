###################################################################################################
#### Script to prepare current annual hydrology data for water hole persistence modelling
#### C. James...............9th January 2012

# Load in necessary libraries
library(SDMTools)

# Set up directories
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
accum.dir = "/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/"
runoff.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/"
out.dir ="/home/jc246980/Hydrology.trials/Hydrology metrics/Metrics_future_1960base/"

wd ="/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1960to1990/"
current_dynamic_accumulated=read.csv(paste(wd, "Current_dynamic.csv", sep=''))

load(paste(runoff.dir,"Current_dynamic.Rdata",sep='')) #loads runoff data
current_dynamic_runoff=Runoff

current_dynamic_accumulated$Annual_sum =rowSums(current_dynamic_accumulated[,c(2:13)]) # determine annual runoff
current_dynamic_runoff$Annual_sum =rowSums(current_dynamic_runoff[,c(2:13)]) # determine annual runoff

Accum_flow=current_dynamic_accumulated[,c("SegmentNo", "Annual_sum")]
Runoff_local=current_dynamic_runoff[,c("SegmentNo", "Annual_sum")]

Hydro_missing=Runoff_local[which(!(Runoff_local$SegmentNo %in% Accum_flow$SegmentNo)),]   
ANNUAL_SUM=rbind(Accum_flow, Hydro_missing)

write.csv(ANNUAL_SUM,paste(out.dir,"Current_annual_sum.csv",sep=''),row.names=F)	

	