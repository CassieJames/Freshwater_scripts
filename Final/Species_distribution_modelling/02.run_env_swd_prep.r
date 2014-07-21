#### Run Script to prepare future environmental data for modelling
#### C. James, L.Hodgson...............9th January 2012

################################################################################
### read in the necessary info
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #retireve and evaluate the arguments
library(maptools)


################################################################################
# Set up directories
accum.dir = "/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/"
runoff.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/"
bioclim.dir ="/home/jc246980/Climate/5km/Future/Bioclim_reach/"
dryseason.dir = "/home/jc246980/DrySeason/DrySeason_reach/"
basedir = '/home/jc246980/SDM/raw_data/'
out.dir='/home/jc246980/SDM/Environmental_future/'
terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')
cois=c('SEGMENTNO', 'VALLEYSLOP', 'CATSLOPE','D2OUTLET')
terrain_sub=terrain[,cois]
colnames(terrain_sub)=c("SegmentNo", "Segslope", "Catslope", "d2outlet")

###load in necessary data
if (file.exists(paste(basedir,'current_flow_data_for_future.Rdata',sep=''))) {
load(paste(basedir,'current_flow_data_for_future.Rdata',sep='')) #load in the current data
} else { #create the necessary current dataset
current_dynamic=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_dynamic.csv")
current_static=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_static.csv")
currents=cbind(current_dynamic,current_static[,-1]) #the two current runoff files are in the same order by SegmentNo and can therefore be joined with cbind
rm(list=c("current_dynamic","current_static"));gc()
save(currents,file=paste(basedir,'current_flow_data_for_future.Rdata',sep='')) #save out the data
}
bioclim = read.csv(paste(bioclim.dir,es,"_",gcm,".csv", sep='')) #read in the bioclim data
for (tt in paste('bioclim_',sprintf('%02i',c(1,4:11)),sep="")) { cat(tt,'\n') #round temperature where appropriate
cois = grep(tt,colnames(bioclim)); bioclim[,cois] = round(bioclim[,cois],1)
}
for (tt in paste('bioclim_',sprintf('%02i',c(12:14,16:19)),sep="")) { cat(tt,'\n')#round precip where appropriate
cois = grep(tt,colnames(bioclim)); bioclim[,cois] = round(bioclim[,cois])
}
load(paste(runoff.dir,es,"_",gcm,".Rdata",sep='')) #loads runoff data
VOIS=c("num_month", "total_severity", "max_clust_length","fut_clust_severity", "fut_month_max_clust") #dry season variables
for (voi in VOIS) { cat(voi,'\n') #loop through to load them in
assign(voi,read.csv(paste(dryseason.dir,es,"_",gcm,"_",voi,".csv",sep=''))) #loads dry season metrics
}

###work with the data
YEARs=seq(2015, 2085, 10) #define the years to work through
for (yy in YEARs) { cat(yy,'\n') #cycle through eacy of the years
cois= c(1,grep(yy,names(bioclim)))# years of interest plus SegmentNo for the bioclim data
out = bioclim[,cois]; out$lat = out$lon = out$SegmentNo;
out = out[,c('SegmentNo','lat','lon',colnames(out)[grep('bioclim',colnames(out))])] #define the output data replicating segment number as lat and lon
colnames(out) = c('SegmentNo','lat','lon',paste('bioclim_',sprintf('%02i',c(1:19)),sep=""))
cois= c(1,grep(yy,names(Runoff)))#columns of interest plus SegmentNo for the runoff data
runoff.data= Runoff[,cois]; runoff.data$Annual_mean =rowSums(runoff.data[,c(2:13)]) # determine annual mean runoff
accum.data= read.csv(paste(accum.dir,es,"_",gcm,".",yy,".csv",sep='')) #loads acummulated data
tdata=merge(currents, accum.data, by="SegmentNo")
tdata$Annual_mean=rowSums((tdata[,c(2:13)]+1)*((tdata[,c(26:37)])/(tdata[,c(14:25)]+1))) # apply correction
Accum_flow=tdata[,c("SegmentNo", "Annual_mean")]
Runoff_local=runoff.data[,c("SegmentNo", "Annual_mean")]
Hydro_missing=Runoff_local[which(!(Runoff_local$SegmentNo %in% Accum_flow$SegmentNo)),]
ANNUAL_MEAN=rbind(Accum_flow, Hydro_missing)
colnames(ANNUAL_MEAN)[2]='Flow_accum_annual'
Enviro_dat = num_month[,c(1,grep(yy,colnames(num_month)))] #get the num months
Enviro_dat = cbind(Enviro_dat,total_severity[,grep(yy,colnames(total_severity))]) #append total severity
Enviro_dat = cbind(Enviro_dat,max_clust_length[,grep(yy,colnames(max_clust_length))]) #append max_clust_length
Enviro_dat = cbind(Enviro_dat,fut_clust_severity[,grep(yy,colnames(fut_clust_severity))]) #append fut_clust_severity
Enviro_dat = cbind(Enviro_dat,fut_month_max_clust[,grep(yy,colnames(fut_month_max_clust))]) #append fut_month_max_clust
colnames(Enviro_dat) = c('SegmentNo',"num.month", "total.severity", "max.clust.length","clust.severity", "month.max.clust") #define the column names
out = merge(out,Enviro_dat); out = merge(out,ANNUAL_MEAN) #fully define the output
out=merge(out,terrain_sub)

write.csv(out,paste(out.dir,es,"_",gcm,"_",yy,".csv",sep=''),row.names=FALSE)#write out the data
rm(list=c("runoff.data","accum.data","tdata","Accum_flow","Runoff_local","Hydro_missing","Enviro_dat","out"));gc() #cleanup extra files
}