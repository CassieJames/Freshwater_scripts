###################################################################################################
#### Script to map all turtle data onto reaches

library(SDMTools); library(maptools) #load the necessary library

craydata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Inverts/Invert_species/" # crayfish point data directory
craydatabase= "/home/jc246980/Species_data/Crayfish_database/Crayfish_species/"
crayfishadded="/home/jc246980/Species_data/Crayfish_database/Additional_records/"



out.dir="/home/jc246980/Species_data/Reach_data/"
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Cray_data = matrix(NA, nrow=nrow(networkatts), ncol=115)
Cray_data [,1] <- networkatts[,9]
species=list.files(craydata.dir)
species2=list.files(craydatabase)
species3=list.files(crayfishadded)

full.list=unique((c(species, species2, species3)))

speciesname=gsub('.csv','',full.list)		
colnames(Cray_data)=c("SegmentNo", speciesname)
#species that will have to be removed: # Cherax depressus, Cherax_neocarinatus, Cherax quinquecarinatus

outsummary = matrix(NA,nrow=length(full.list),ncol=6); #define the output matrix
colnames(outsummary)=c("Species", "ALA_records","ALA_unique_Segments","other_records", "other_unique_segments", "Unique segments")
rownames(outsummary)=full.list

	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		SegmentNo_SP_Present_ala=NULL
		SegmentNo_SP_Present_other=NULL
		species.data.ala=NULL
		species.data.db=NULL
		species.data.add=NULL
		
		#pull in data from various sources					  
		if(file.exists(paste(craydata.dir,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(craydata.dir,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.ala$Latitude_processed
				species.data.longdec=species.data.ala$Longitude_processed
				SegmentNo_SP_Present_ala  = extract.data(cbind(species.data.ala$Longitude_processed,species.data.ala$Latitude_processed),CatchmentRaster.asc) # extract Segment number from Catchment raster
		}
		
		
		if(file.exists(paste(craydatabase,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.db$LATDEC*-1
				species.data.longdec=species.data.db$LONGDEC
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		}
	  
		if(file.exists(paste(crayfishadded,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.add$Lat
				species.data.longdec=species.data.add$Long
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		}
		
		
		if(file.exists(paste(craydatabase,"/", full.list[sp],sep=''))&& file.exists(paste(crayfishadded,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(craydatabase,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(crayfishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.db$LONGDEC, species.data.add$Long)
				species.data.latdec=c((species.data.db$LATDEC*-1), species.data.add$Lat)
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		}
		
		if(file.exists(SegmentNo_SP_Present_other){
		Number_ALA = 0
		Unique_segs_ALA = 0
		Number_other = length(na.omit(SegmentNo_SP_Present_other)) 
		Unique_segs_other = length(unique(na.omit(SegmentNo_SP_Present_other))) 
		Unique_segs=c(SegmentNo_SP_Present_other)
		Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_other)))) 
		}
		
		if(file.exists(SegmentNo_SP_Present_ala){
		Number_ALA = length(na.omit(SegmentNo_SP_Present_ala)) 
		Unique_segs_ALA = length(unique(na.omit(SegmentNo_SP_Present_ala))) 
		Number_other = 0 
		Unique_segs_other = 0
		Unique_segs=c(SegmentNo_SP_Present_ala)
		Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_ala)))) 
		}
	
		if(file.exists(SegmentNo_SP_Present_ala) && file.exists(SegmentNo_SP_Present_other)) {
		Number_ALA = length(na.omit(SegmentNo_SP_Present_ala)) 
		Unique_segs_ALA = length(unique(na.omit(SegmentNo_SP_Present_ala))) 
		Number_other = length(na.omit(SegmentNo_SP_Present_other)) 
		Unique_segs_other = length(unique(na.omit(SegmentNo_SP_Present_other))) 
		Unique_segs=c(SegmentNo_SP_Present_ala,SegmentNo_SP_Present_other)
		Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_ala,SegmentNo_SP_Present_other)))) 
		}
		outsummary[grep(full.list[sp],rownames(outsummary)),]=c(speciesname,Number_ALA,Unique_segs_ALA,Number_other,Unique_segs_other, Unique_segs_length)
	
		}	

	out.dir="/home/jc246980/Species_data/Reach_data/"
	write.csv(outsummary,paste(out.dir,"Crayfish_summary_statistics.csv",sep=''),row.names=T)	 