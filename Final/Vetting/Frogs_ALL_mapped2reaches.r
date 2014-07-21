###################################################################################################
#### Script to map all frog data onto reaches

library(SDMTools); library(maptools) #load the necessary library

frogdata.dir = "/home/jc214262/Refugia/Vert_data/cleaned_Merged_frog_data/"
out.dir="/home/jc246980/Species_data/Reach_data/"
files=list.files(frogdata.dir)

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Frog_data = matrix(NA, nrow=nrow(networkatts), ncol=length(files)+1)
Frog_data [,1] <- networkatts[,9]
speciesname=gsub('.csv','',files)		
colnames(Frog_data)=c("SegmentNo", speciesname)


	for (sp in 1:length(files)) { cat(files[sp],'\n')
		speciesname=as.vector(files[sp])
		species.data.ala = read.csv(paste(frogdata.dir,files[sp],sep='')) # read in ALA data

		species.data.latdec=species.data.ala$LATDEC
		species.data.longdec=species.data.ala$LONGDEC
	  
		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Frog_data=as.data.frame(Frog_data)
		Frog_data[(Frog_data$SegmentNo %in% SegmentNo_SP_Present),gsub('.csv','',speciesname)] <- 1
		Frog_data[!(Frog_data$SegmentNo %in% SegmentNo_SP_Present),gsub('.csv','',speciesname)] <- 0
		
		}	

write.csv(Frog_data,paste(out.dir,'Frog_ALL_reach.csv',sep=''),row.names=F)
save(Frog_data,file=paste(out.dir,"Frog_ALL_reach.Rdata",sep=''))

#write out Frog list
write.csv(speciesname,paste(out.dir,'Full_ALL_Frog_list.csv',sep=''),row.names=F)

