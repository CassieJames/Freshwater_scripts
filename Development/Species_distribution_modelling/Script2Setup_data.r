###################################################################################################
### Script to setup files for MAXENT run trials

library(SDMTools) #define the libraries needed

### read in data (need to read in runoff and get runoff values for segments that don't occur in hydro
hydro=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv")
hydro$MeanAnnual=rowMeans(hydro[,2:13])

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata")
Runoff$MeanAnnual=rowMeans(Runoff[,2:13])
hydro_extra=Runoff[which(!(Runoff$SegmentNo %in% hydro$SegmentNo)),]   

HYDRO=hydro[,c(1,14)]
HYDRO_EXTRA=hydro_extra[, c(1,14)]
HYDROLOGY=rbind(HYDRO,HYDRO_EXTRA)

bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv")

load("/home/jc246980/Zonation/Fish_reach_aggregated.Rdata")