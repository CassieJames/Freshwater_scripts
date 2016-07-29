###################################################################################################
### Script to setup current files for MAXENT run trials

library(SDMTools)
library(maptools) #define the libraries needed

### set up input data
hydro=read.csv("/home/jc246980/Obsolete/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv") # read in accumulated flow
load("/home/jc246980/Obsolete/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata") # read in runoff
bioclim=read.csv("/home/jc246980/Obsolete/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv") # read in bioclim variables

terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')
cois=c('SEGMENTNO', 'VALLEYSLOP', 'CATSLOPE','D2OUTLET')
terrain_sub=terrain[,cois]
colnames(terrain_sub)=c("SegmentNo", "Segslope", "Catslope", "d2outlet")

out = bioclim; out$lat = out$lon = out$SegmentNo; out = out[,c('SegmentNo','lat','lon',colnames(bioclim)[-1])] #define the output data replicating segment number as lat and lon

### Create annual means and fill in accumulated flow gaps
hydro$MeanAnnual=rowSums(hydro[,2:13])
Runoff$MeanAnnual=rowSums(Runoff[,2:13])
hydro_extra=Runoff[which(!(Runoff$SegmentNo %in% hydro$SegmentNo)),]
HYDRO=hydro[,c(1,14)]
HYDRO_EXTRA=hydro_extra[, c(1,14)]
HYDROLOGY=rbind(HYDRO,HYDRO_EXTRA); colnames(HYDROLOGY)[2]='Flow_accum_annual'

out = merge(out,HYDROLOGY) #fully define the output
out=merge(out,terrain_sub)

current = out; save(current,file='/home/jc246980/SDM_SIMP/current_enviro_data.Rdata') #write out the data