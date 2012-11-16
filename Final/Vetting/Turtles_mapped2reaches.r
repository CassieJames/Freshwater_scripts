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


Turtle_data = matrix(NA, nrow=nrow(networkatts), ncol=29)
Turtle_data [,1] <- networkatts[,9]
species=list.files(turtledata.dir)
speciesname=gsub('.csv','',species)		
colnames(Turtle_data)=c("SegmentNo", speciesname)


	for (sp in 1:length(species)) { cat(species[sp],'\n')
		speciesname=gsub('.csv','',species[sp])
		species.data.ala = read.csv(paste(turtledata.dir,species[sp],sep='')) # read in ALA data

					  
		if(file.exists(paste(UCturtledata,"/", species[sp],sep=''))){
				species.data.UC=read.csv(paste(UCturtledata,"/",species[sp],sep=''))
				species.data.latdec=c(species.data.ala$LATDEC, (species.data.UC$DECLAT)*-1)
				species.data.longdec=c(species.data.ala$LONGDEC, species.data.UC$DECLONG)
		}else{
				species.data.latdec=species.data.ala$LATDEC
				species.data.longdec=species.data.ala$LONGDEC
		}
	  
		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Turtle_data=as.data.frame(Turtle_data)
		Turtle_data[(Turtle_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Turtle_data[!(Turtle_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	

	write.csv(Turtle_data,paste(out.dir,'Turtles_reach.csv',sep=''),row.names=F)