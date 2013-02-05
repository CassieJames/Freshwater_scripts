###################################################################################################
#### Script to map all turtle data onto reaches

library(SDMTools); library(maptools) #load the necessary library

turtledata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Turtles/" # turtle point data directory
UCturtledata= "/home/jc246980/Species_data/UC_Wildlife_Tissue_Collection/"

out.dir="/home/jc246980/Species_data/Reach_data/"
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Turtle_data = matrix(NA, nrow=nrow(networkatts), ncol=30) # IF the species list is changed MAKE SURE the matrix is adjusted!
Turtle_data [,1] <- networkatts[,9]
species=list.files(turtledata.dir)
species2=list.files(UCturtledata, pattern='csv')
full.list=unique(c(species, species2))

speciesname=gsub('.csv','',full.list)		
colnames(Turtle_data)=c("SegmentNo", speciesname)

outsummary = matrix(NA,nrow=length(full.list),ncol=6); #define the output matrix
colnames(outsummary)=c("Species", "ALA_records","ALA_unique_Segments","other_records", "other_unique_segments", "Unique segments")
rownames(outsummary)=full.list

	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		SegmentNo_SP_Present_ala=NULL
		SegmentNo_SP_Present_other=NULL
		species.data.ala=NULL
		species.data.UC=NULL
	
			
		if(file.exists(paste(turtledata.dir,"/", full.list[sp],sep=''))){
			species.data.ala=read.csv(paste(turtledata.dir,full.list[sp],sep=''))
			species.data.latdec=species.data.ala$LATDEC
			species.data.longdec=species.data.ala$LONGDEC
			SegmentNo_SP_Present_ala  = extract.data(cbind(species.data.ala$LONGDEC,species.data.ala$LATDEC),CatchmentRaster.asc) # extract Segment number from Catchment raster
		}
					  
		if(file.exists(paste(UCturtledata,"/", full.list[sp],sep=''))){
			species.data.UC=read.csv(paste(UCturtledata,"/",full.list[sp],sep=''))
			species.data.latdec=species.data.UC$DECLAT*-1
			species.data.longdec=species.data.UC$DECLONG
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
	write.csv(outsummary,paste(out.dir,"Turtle_summary_statistics.csv",sep=''),row.names=T)	 
	