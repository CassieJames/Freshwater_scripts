###################################################################################################
#### Script to summarise fish species data

library(SDMTools); library(maptools) #load the necessary library


### read in catchment asc at 250 m resolution, create pos250 and append segmentno info


load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]  

### Fish

fishdata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Fish/" # fish point data directory from ALA
fishatlas.dir= "/home/jc246980/Species_data/NorthernOZFish/Data/"
fishadded="/home/jc246980/Species_data/NorthernOZFish/Additional_Data/"

species=list.files(fishdata.dir, pattern=".csv")
species2=list.files(fishatlas.dir)
species3=list.files(fishadded)

full.list=unique(c(species, species2, species3))

length(full.list)

full.list=unique(c(species, species2, species3))

# write.csv(full.list,paste(out.dir,'Fish_full_list.csv',sep=''),row.names=F) # write out full species list to check spelling consistencies

# speciesname=gsub('.csv','',full.list)		
# colnames(Fish_data)=c("SegmentNo", speciesname)

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
		if(file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.ala$LATDEC
				species.data.longdec=species.data.ala$LONGDEC
				SegmentNo_SP_Present_ala  = extract.data(cbind(species.data.ala$LONGDEC,species.data.ala$LATDEC),CatchmentRaster.asc) # extract Segment number from Catchment raster
				Number_ALA = length(na.omit(SegmentNo_SP_Present_ala)) 
				Unique_segs_ALA = length(unique(na.omit(SegmentNo_SP_Present_ala))) 
				Number_other = 0 
				Unique_segs_other = 0
				Unique_segs=c(SegmentNo_SP_Present_ala)
				Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_ala)))) 
		
		}
			
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.latdec=as.numeric(species.data.db$LATITUDE)
				species.data.longdec=as.numeric(species.data.db$LONGITUDE)
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
				Number_ALA = 0
				Unique_segs_ALA = 0
				Number_other = length(na.omit(SegmentNo_SP_Present_other)) 
				Unique_segs_other = length(unique(na.omit(SegmentNo_SP_Present_other))) 
				Unique_segs=c(SegmentNo_SP_Present_other)
				Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_other)))) 
		}
	  
		if(file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.add$Lat
				species.data.longdec=species.data.add$Long
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
				Number_ALA = 0
				Unique_segs_ALA = 0
				Number_other = length(na.omit(SegmentNo_SP_Present_other)) 
				Unique_segs_other = length(unique(na.omit(SegmentNo_SP_Present_other))) 
				Unique_segs=c(SegmentNo_SP_Present_other)
				Unique_segs_length = length(unique(na.omit(c(SegmentNo_SP_Present_other)))) 
		}
		
		
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))&& file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.db$LONGITUDE, species.data.add$Long)
				species.data.latdec=c((species.data.db$LATITUDE), species.data.add$Lat)
				SegmentNo_SP_Present_other  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
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
	write.csv(outsummary,paste(out.dir,"Fish_summary_statistics.csv",sep=''),row.names=T)	 