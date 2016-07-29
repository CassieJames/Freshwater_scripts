###################################################################################################
#### Script to map all turtle data onto reaches

library(SDMTools); library(maptools) #load the necessary library

crabdata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Crab/" # turtle point data directory


out.dir="/home/jc246980/Species_data/Reach_data/"
wd = "/home/jc246980/Obsolete/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Obsolete/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Crab_data = matrix(NA, nrow=nrow(networkatts), ncol=2) # IF the species list is changed MAKE SURE the matrix is adjusted!
Crab_data [,1] <- networkatts[,9]
species=list.files(crabdata.dir)

full.list=unique(c(species))

speciesname=gsub('.csv','',full.list)		
colnames(Crab_data)=c("SegmentNo", speciesname)


	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		species.data.ala=NULL
		
		if(file.exists(paste(crabdata.dir,"/", full.list[sp],sep=''))){
			species.data.ala=read.csv(paste(crabdata.dir,full.list[sp],sep=''))
			species.data.latdec=species.data.ala$LATDEC
			species.data.longdec=species.data.ala$LONGDEC
		}
					  
		SegmentNo_SP_Present = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Crab_data=as.data.frame(Crab_data)
		Crab_data[(Crab_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Crab_data[!(Crab_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	

	write.csv(Crab_data,paste(out.dir,'Crab_reach.csv',sep=''),row.names=F)
	save(Crab_data,file=paste(out.dir,"Crab_reach.Rdata",sep=''))
	
### retrieve occurs only

occur.file="/home/jc246980/Species_data/Reach_data/Crab_reach.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur=occur[which(occur$Austrothelphusa_transversa>0),] #remove SegmentNos (rows) with no occurrence records for any species

save(occur,file=paste(out.dir,"Crab_reach_master.Rdata",sep=''))
	