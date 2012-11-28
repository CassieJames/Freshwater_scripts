###File set up for species models trials 1976-2005
library(maptools)
###################################################################################################
###Current

bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv")
runoff=load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata")
accum_runoff=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')
hydro2Seg=networkatts[,c("SegmentNo", "HydroID")]

env_data=bioclim[,c(1, 2,5,13,16)]

###append annual local runoff and sd
Runoff$annual=rowSums(Runoff[,c(2:13)])
Runoff$Monthly_sd=apply(Runoff[,c(2:13)],1,sd)
env_data=merge(env_data, Runoff[,c("SegmentNo","annual","Monthly_sd")], by="SegmentNo")
colnames(env_data)[6]="local_runoff"
colnames(env_data)[7]="local_runoff_sd"

###Append accumulated runoff

accum_runoff$annual_sum=rowSums(accum_runoff[,2:13])
accum_runoff$annual_sd=apply(accum_runoff[,2:13],1,sd)
env_data=merge(env_data, accum_runoff[,c("SegmentNo","annual_sum","annual_sd")], by="SegmentNo")
colnames(env_data)[8]="Accum_runoff"
colnames(env_data)[9]="Accum_runoff_sd"

out.dir = "/home/jc246980/SDM/Models/"
write.csv(env_data,paste(out.dir,"env_data_trial_current.csv",sep=''),row.names=F)	

###################################################################################################
###Future

bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/RCP85_csiro-mk30.csv")
cois=NULL
cois = c(cois,grep(2085, colnames(bioclim)))
bioclim_yoi = bioclim[,c(1,cois)]
env_data=bioclim[,c(1, 2,5,13,16)]

runoff=load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/RCP85_csiro-mk30.Rdata")
cois=NULL
cois = c(cois,grep(2085, colnames(Runoff)))
Runoff_yoi=Runoff[,c(1,cois)]
Runoff_yoi$annual=rowSums(Runoff_yoi[,c(2:13)])
Runoff_yoi$Monthly_sd=apply(Runoff_yoi[,c(2:13)],1,sd)
env_data=merge(env_data, Runoff_yoi[,c("SegmentNo","annual","Monthly_sd")], by="SegmentNo")
colnames(env_data)[6]="local_runoff"
colnames(env_data)[7]="local_runoff_sd"

accum_runoff=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/RCP85_csiro-mk30.2085.csv")
accum_runoff$annual_sum=rowSums(accum_runoff[,2:13])
accum_runoff$annual_sd=apply(accum_runoff[,2:13],1,sd)
env_data=merge(env_data, accum_runoff[,c("SegmentNo", "annual_sum", "annual_sd")], by="SegmentNo")
colnames(env_data)[8]="Accum_runoff"
colnames(env_data)[9]="Accum_runoff_sd"

write.csv(env_data,paste(out.dir,"env_data_RCP85_csiro-mk30.2085.csv",sep=''),row.names=F)	











