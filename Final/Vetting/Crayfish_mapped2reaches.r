###################################################################################################
#### Script to map all crayfish data onto reaches

library(SDMTools); library(maptools) #load the necessary library

craydata.dir = "/home/jc246980/Species_data/Crayfish_database/Merged_crayfish_records/" # crayfish point data directory
out.dir="/home/jc246980/Species_data/Reach_data/"

#wd = "/home/jc246980/Obsolete/Hydrology.trials/"; setwd(wd)                        
load("/home/jc246980/Obsolete/Hydrology.trials/Catchmentraster250.Rdata")

networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')

Cray_data = matrix(NA, nrow=nrow(networkatts), ncol=136)
Cray_data [,1] <- networkatts[,9]

full.list = read.csv("/home/jc246980/Species_data/Crayfish_database/Full_crayfish_list.csv")
full.list=full.list[,1]
speciesname=gsub('.csv','',full.list)		
colnames(Cray_data)=c("SegmentNo", speciesname)
#species that will have to be removed: # Cherax depressus, Cherax_neocarinatus, Cherax quinquecarinatus


	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		species.data=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
		species.data.latdec=species.data$LAT
		species.data.longdec=species.data$LONG
		
	
		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Cray_data=as.data.frame(Cray_data)
		Cray_data[(Cray_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Cray_data[!(Cray_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	


	write.csv(Cray_data,paste(out.dir,'Crayfish_reach.csv',sep=''),row.names=F)
	save(Cray_data,file=paste(out.dir,"Crayfish_reach.Rdata",sep=''))
	
	write.csv(full.list,paste(out.dir,'Full_crayfish_list.csv',sep=''),row.names=F)
