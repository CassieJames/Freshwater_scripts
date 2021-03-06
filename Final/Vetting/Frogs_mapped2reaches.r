###################################################################################################
#### Script to map all frog data onto reaches

library(SDMTools); library(maptools) #load the necessary library

frogdata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Frogs/"
work.dir="/home/jc246980/Species_data/ALA_downloads/"             # location of full fish species list
out.dir="/home/jc246980/Species_data/Reach_data/"
FrogList = as.data.frame(read.csv(paste(work.dir,"/","FrogsinALA.csv",sep=""), header = T,sep = ",", fill = T))   # load species list
species=FrogList$Species
species=species[-c(15)] # remove Nyctimystes as no records in ALA

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Frog_data = matrix(NA, nrow=nrow(networkatts), ncol=26)
Frog_data [,1] <- networkatts[,9]
speciesname=gsub('.csv','',species)		
colnames(Frog_data)=c("SegmentNo", speciesname)


	for (sp in 1:length(species)) { cat(species[sp],'\n')
		speciesname=as.vector(species[sp])
		species.data.ala = read.csv(paste(frogdata.dir,species[sp],".csv",sep='')) # read in ALA data

		species.data.latdec=species.data.ala$LATDEC
		species.data.longdec=species.data.ala$LONGDEC
	  
		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Frog_data=as.data.frame(Frog_data)
		Frog_data[(Frog_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Frog_data[!(Frog_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	

write.csv(Frog_data,paste(out.dir,'Frog_reach.csv',sep=''),row.names=F)
save(Frog_data,file=paste(out.dir,"Frog_reach.Rdata",sep=''))

#write out Frog list
write.csv(species,paste(out.dir,'Full_Frog_list.csv',sep=''),row.names=F)

