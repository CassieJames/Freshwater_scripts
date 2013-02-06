###################################################################################################
#### Script to prepare future environmental data for modelling
#### C. James...............9th January 2012

# Load in necessary libraries
library(SDMTools)

# Set up directories
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
accum.dir = "/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/"
runoff.dir="/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/"
bioclim.dir ="/home/jc246980/Climate/5km/Future/Bioclim_reach/"
dryseason.dir = "/home/jc246980/DrySeason/DrySeason_reach/"

out.dir ="/home/jc246980/SDM/Environmental_future/"

wd ="/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/"

current_dynamic=read.csv(paste(wd, "Current_dynamic.csv", sep=''))
current_static=read.csv(paste(wd, "Current_static.csv", sep=''))
currents=merge(current_static, current_dynamic,by="SegmentNo")

ESs = list.files(futdir, pattern="RCP") 										# list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep=''))									# get a list of GCMs
YEAR=seq(2015, 2085, 10)	

	for (es in ESs) {
	
		for (gcm in GCMs) {
			
			load(paste(runoff.dir,es,"_",gcm,".Rdata",sep='')) #loads runoff data
			
			num_month=read.csv(paste(dryseason.dir,es,"_",gcm,"_num_month.csv",sep='')) #loads dry season metrics
			total_severity=read.csv(paste(dryseason.dir,es,"_",gcm,"_total_severity.csv",sep='')) #loads dry season metrics			
			clust_length=read.csv(paste(dryseason.dir,es,"_",gcm,"_max_clust_length.csv",sep='')) #loads dry season metrics
			clust_severity=read.csv(paste(dryseason.dir,es,"_",gcm,"_fut_clust_severity.csv",sep='')) #loads dry season metrics
			fut_month_max_clust=read.csv(paste(dryseason.dir,es,"_",gcm,"_fut_month_max_clust.csv",sep='')) #loads dry season metrics

			bioclim.dat=read.csv(paste(bioclim.dir,es,"_",gcm, ".csv", sep=''))
			
			
			for (yy in YEAR) {
			
				cois= c(1,grep(yy,names(Runoff)))#columns of interest plus SegmentNo
				runoff.data= Runoff[,cois]
				runoff.data$Annual_mean =rowSums(runoff.data[,c(2:13)]) # determine annual mean runoff
				
				accum.data= read.csv(paste(accum.dir,es,"_",gcm,".",yy,".csv",sep='')) #loads acummulated data
				tdata=merge(currents, accum.data, by="SegmentNo")
				tdata$Annual_mean=rowSums((tdata[,c(2:13)]+1)*((tdata[,c(26:37)])/(tdata[,c(14:25)]+1))) # apply correction
				
				Accum_flow=tdata[,c("SegmentNo", "Annual_mean")]
				Runoff_local=runoff.data[,c("SegmentNo", "Annual_mean")]
				Hydro_missing=Runoff_local[which(!(Runoff_local$SegmentNo %in% Accum_flow$SegmentNo)),]   
				ANNUAL_MEAN=rbind(Accum_flow, Hydro_missing)
	
				cois= c(1,grep(yy,names(bioclim.dat)))# years of interest plus SegmentNo
				bioclim=bioclim.dat[,cois]
				vars = c('SegmentNo',paste('bioclim_',sprintf('%02i',c(1:19)),"_",yy,sep='')) # subset to bioclim variables of interest (not subset at the moment
				bioclim=bioclim[,vars]
								
				cois= c(1,grep(yy,names(num_month)))# years of interest plus SegmentNo
				num_month_yy=num_month[,cois]

				cois= c(1,grep(yy,names(total_severity)))# years of interest plus SegmentNo
				total_severity_yy=total_severity[,cois]				
				
				cois= c(1,grep(yy,names(clust_length)))# years of interest plus SegmentNo
				clust_length_yy=clust_length[,cois]				
				
				cois= c(1,grep(yy,names(clust_severity)))# years of interest plus SegmentNo
				clust_severity_yy=clust_severity[,cois]				
				
				cois= c(1,grep(yy,names(fut_month_max_clust)))# years of interest plus SegmentNo
				fut_month_max_clust_yy=fut_month_max_clust[,cois]				
				
				Enviro_dat=merge(bioclim, num_month_yy, by="SegmentNo", all.x=TRUE)
				Enviro_dat=merge(Enviro_dat, total_severity_yy, by="SegmentNo", all.x=TRUE)
				Enviro_dat=merge(Enviro_dat, clust_length_yy, by="SegmentNo", all.x=TRUE)
				Enviro_dat=merge(Enviro_dat, clust_severity_yy, by="SegmentNo", all.x=TRUE)
				Enviro_dat=merge(Enviro_dat, fut_month_max_clust_yy, by="SegmentNo", all.x=TRUE)
				Enviro_dat=merge(Enviro_dat, ANNUAL_MEAN, by="SegmentNo", all.x=TRUE)
				
				tt=c('SegmentNo',paste('bioclim_',sprintf('%02i',c(1:19)),sep=""),'num_month', 'total_severity', 'clust_length', 'clust_severity', 'month_max_clust', 'annual_flow')
				
				colnames(Enviro_dat)=tt
				
				
				write.csv(Enviro_dat,paste(out.dir,es,"_",gcm,"_",yy,".csv",sep=''),row.names=F)	
			}
		}
	}

