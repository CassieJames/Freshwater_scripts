###################################################################################################
#### Script to map all turtle data onto reaches

library(SDMTools); library(maptools) #load the necessary library

fishdata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Fish/" # fish point data directory from ALA
fishatlas.dir= "/home/jc246980/Species_data/NorthernOZFish/Data/"
fishadded="/home/jc246980/Species_data/NorthernOZFish/Additional_Data/"


###Script to create csv files for additional fish data from literature

# database.dir="/home/jc246980/Species_data/NorthernOZFish/"
# fishdata = read.csv(paste(database.dir, '/',"Additional_data_fish.csv",sep=''))
# species=colnames(fishdata[,8:102])

      # for (sp in species) { cat(sp,'\n')

                   # Fish <- fishdata[which(fishdata[,sp] ==1),1:7]
                   
                   # write.csv(Fish,paste("/home/jc246980/Species_data/NorthernOZFish/Additional_Data/",sp,".csv", sep = ''), row.names = F )

      # }

	

out.dir="/home/jc246980/Species_data/Reach_data/"
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                        
#CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))  
#save(CatchmentRaster.asc,file=paste(wd,'Catchmentraster250.Rdata',sep=''))
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')


Fish_data = matrix(NA, nrow=nrow(networkatts), ncol=363)
Fish_data [,1] <- networkatts[,9]
species=list.files(fishdata.dir, pattern=".csv")
species2=list.files(fishatlas.dir)
species3=list.files(fishadded)

full.list=unique(c(species, species2, species3))

#write.csv(full.list,paste(out.dir,'Fish_full_list.csv',sep=''),row.names=F) # write out full species list to check spelling consistencies

speciesname=gsub('.csv','',full.list)		
colnames(Fish_data)=c("SegmentNo", speciesname)



	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		
		species.data.ala=NULL
		species.data.db=NULL
		species.data.add=NULL
		
		#pull in data from various sources					  
		if(file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.ala$LATDEC
				species.data.longdec=species.data.ala$LONGDEC
		}
		
		
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.db$LATITUDE
				species.data.longdec=species.data.db$LONGITUDE
		}
	  
		if(file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.latdec=species.data.add$Lat
				species.data.longdec=species.data.add$Long
		}
		
		if(file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))&& file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$LONGDEC, species.data.db$LONGITUDE)
				species.data.latdec=c(species.data.ala$LATDEC, (species.data.db$LATITUDE))
		}
		
		if(file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))&& file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.ala=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$LONGDEC, species.data.add$Long)
				species.data.latdec=c(species.data.ala$LATDEC, species.data.add$Lat)
		}
		
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))&& file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.db$LONGITUDE, species.data.add$Long)
				species.data.latdec=c((species.data.db$LATITUDE), species.data.add$Lat)
		}
		
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))&& file.exists(paste(fishadded,"/", full.list[sp],sep='')) && file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))){
				
				species.data.ala=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.db=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.longdec=c(species.data.ala$LONGDEC, species.data.db$LONGITUDE, species.data.add$Long)
				species.data.latdec=c(species.data.ala$LATDEC, (species.data.db$LATITUDE), species.data.add$Lat)
		}
		

		SegmentNo_SP_Present  = extract.data(cbind(species.data.longdec,species.data.latdec),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		
		Fish_data=as.data.frame(Fish_data)
		Fish_data[(Fish_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Fish_data[!(Fish_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
		}	


	write.csv(Fish_data,paste(out.dir,'Fish_reach.csv',sep=''),row.names=F)