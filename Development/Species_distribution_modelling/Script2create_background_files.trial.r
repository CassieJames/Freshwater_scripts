###File set up for species models trials 1976-2005

###################################################################################################
###Current

bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv")

runoff=load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_dynamic.Rdata")

accum_runoff=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv")

env_data=bioclim[,c(1, 2,5,13,16)]

tdata=rowSums(Runoff)

tdata=as.data.frame(tdata)
env_data=cbind(env_data, tdata)

colnames(env_data)[6]="local_runoff"

tdata=apply(Runoff,1,sd)

env_data=cbind(env_data, tdata)

colnames(env_data)[7]="local_runoff_sd"

accum_runoff$annual_sum=rowSums(accum_runoff[,2:13])

accum_runoff$annual_sd=apply(accum_runoff[,2:13],1,sd)

env_data=merge(env_data, accum_runoff[,c("SegmentNo","annual_sum","annual_sd")], by="SegmentNo")

colnames(env_data)[8]="Accum_runoff"
colnames(env_data)[9]="Accum_runoff_sd"

out.dir = "/home/jc246980/SDM/Models/"

write.csv(env_data,paste(out.dir,"env_data_trial.csv",sep=''),row.names=F)	

###################################################################################################
###Future

bioclim=read.csv("/home/jc246980/Climate/5km/Future/Bioclim_reach/RCP85_cccma-cgcm31.csv")
cois=NULL
cois = c(cois,grep(2085, colnames(bioclim)))
bioclim_yoi = bioclim[,c(1,cois)]
env_data=bioclim[,c(1, 2,5,13,16)]

mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

runoff=load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/RCP85_cccma-cgcm31.Rdata")

	for(month in mm){

	cois=NULL
	cois = c(cois,grep(month, colnames(Runoff)))


















