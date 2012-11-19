###################################################################################################
#### Script to map all turtle data onto reaches

library(SDMTools); library(maptools) #load the necessary library

craydata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Inverts/Invert_species/" # turtle point data directory
craydatabase= "/home/jc246980/Species_data/Crayfish_database/Crayfish_species/"
crayfishadded="/home/jc246980/Species_data/Crayfish_database/Additional_records/"



out.dir="/home/jc246980/Species_data/Reach_data/"
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Cray_data = matrix(NA, nrow=nrow(networkatts), ncol=140)
Cray_data [,1] <- networkatts[,9]
species=list.files(craydata.dir)
species2=list.files(craydatabase)
species3=list.files(crayfishadded)

full.list=unique(c(species, species2, species3))

speciesname=gsub('.csv','',full.list)		
colnames(Cray_data)=c("SegmentNo", speciesname)


	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		species.data.ala=NULL
		species.data.db=NULL
		species.data.add=NULL
		
		#pull in data from various sources					  
		if(file.exists(paste(craydata.dir,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.ala$Latitude_processed
				species.data.longdec=species.data.ala$Longitude_processed
		}
		
		
		if(file.exists(paste(craydatabase,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.db$LATDEC*-1
				species.data.longdec=species.data.db$LONGDEC
		}
	  
		if(file.exists(paste(crayfishadded,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.add$Lat
				species.data.longdec=species.data.add$Long
		}
		
		if(file.exists(paste(craydata.dir,"/", full.list[sp],sep=''))&& file.exists(paste(craydatabase,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$Longitude_processed, species.data.db$LONGDEC)
				species.data.latdec=c(species.data.ala$Latitude_processed, (species.data.db$LATDEC*-1))
		}
		
		if(file.exists(paste(craydata.dir,"/", full.list[sp],sep=''))&& file.exists(paste(crayfishadded,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$Longitude_processed, species.data.add$Long)
				species.data.latdec=c(species.data.ala$Latitude_processed, species.data.add$Lat)
		}
		
		if(file.exists(paste(craydatabase,"/", full.list[sp],sep=''))&& file.exists(paste(crayfishadded,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.db$LONGDEC, species.data.add$Long)
				species.data.latdec=c((species.data.db$LATDEC*-1), species.data.add$Lat)
		}
		
		if(file.exists(paste(craydatabase,"/", full.list[sp],sep=''))&& file.exists(paste(crayfishadded,"/", full.list[sp],sep='')) && file.exists(paste(craydata.dir,"/", full.list[sp],sep=''))){
				
				species.data.ala=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$Longitude_processed, species.data.db$LONGDEC, species.data.add$Long)
				species.data.latdec=c(species.data.ala$Latitude_processed, (species.data.db$LATDEC*-1), species.data.add$Lat)
		}
		

		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Cray_data=as.data.frame(Cray_data)
		Cray_data[(Cray_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Cray_data[!(Cray_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	

	write.csv(Cray_data,paste(out.dir,'Crayfish_reach.csv',sep=''),row.names=F)