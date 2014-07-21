###################################################################################################
### Script to setup current files for MAXENT run trials

library(SDMTools)
library(maptools) #define the libraries needed

### set up input data
hydro=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv") # read in accumulated flow
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata") # read in runoff
bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv") # read in bioclim variables
dryseason.dir="/home/jc246980/DrySeason/DrySeason_reach/"
VOIS=c("num.month", "total.severity", "max.clust.length","clust.severity", "month.max.clust")

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

### Create current environmental data file
for(voi in VOIS) { cat(voi,'\n')
tdata=read.csv(paste(dryseason.dir,"Current_",voi,".csv", sep='')) # load data for each varable
if (voi==VOIS[1]) Enviro_dat=tdata else Enviro_dat=merge(Enviro_dat,tdata)
}

out = merge(out,Enviro_dat); out = merge(out,HYDROLOGY) #fully define the output
out=merge(out,terrain_sub)

current = out; save(current,file='/home/jc246980/SDM/current_enviro_data.Rdata') #write out the data