###Script to create species files and map to reaches


library(SDMTools); library(maptools); library(plyr) #load the necessary library


database.dir="/home/jc246980/Species_data/NorthernOZFish/"
fishdata = read.csv(paste(database.dir, '/',"QLDMuseumFish.csv",sep=''))
species=unique(fishdata[,1])
for (sp in species) { cat(sp,'\n')
Fish <- fishdata[which(fishdata[,1] ==sp),]
write.csv(Fish,paste("/home/jc246980/Species_data/NorthernOZFish/QLDmuseum/",sub(" ", "_",sp),".csv", sep = ''), row.names = F )
 }


database.dir="/home/jc246980/Species_data/NorthernOZFish/"
fishdata = read.csv(paste(database.dir, '/',"MarkK_data.csv",sep=''))
species=colnames(fishdata[,3:37])

for (sp in species) { cat(sp,'\n')

Fish <- fishdata[which(fishdata[,sp] ==1),1:2]
write.csv(Fish,paste("/home/jc246980/Species_data/NorthernOZFish/MarkK_data/",sp,".csv", sep = ''), row.names = F )
}





database.dir="/home/jc246980/Species_data/NorthernOZFish/"
fishdata = read.csv(paste(database.dir, '/',"Fish Gilbert and Flinders.csv",sep=''))
species=colnames(fishdata[1,8:ncol(fishdata)])

for (sp in species) { cat(sp,'\n')
Fish <- fishdata[,which(colnames(fishdata) ==sp)]
Fish=cbind(fishdata$Longitude, fishdata$Latitude, Fish)
colnames(Fish)=c("Longitude", "Latitude", "Count")
Fish=as.data.frame(Fish)
Fish=Fish[which(Fish$Count>0),]
Fish[which(Fish$Count>1),3] <- 1
write.csv(Fish,paste("/home/jc246980/Species_data/NorthernOZFish/FGARA_records/",sub(" ", "_",sp),".csv", sep = ''), row.names = F )
 }


### Merge various sources of species info and create a single file for each species

fishdata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Fish/" # fish point data directory from ALA
fishatlas.dir= "/home/jc246980/Species_data/NorthernOZFish/Data/"
fishadded="/home/jc246980/Species_data/NorthernOZFish/Additional_Data/"
fishQLD="/home/jc246980/Species_data/NorthernOZFish/QLDmuseum/"
fishMarkK="/home/jc246980/Species_data/NorthernOZFish/MarkK_data/"
fishFGARA="/home/jc246980/Species_data/NorthernOZFish/FGARA_records/"

species=list.files(fishdata.dir, pattern=".csv")
species2=list.files(fishatlas.dir)
species3=list.files(fishadded)
species5=list.files(fishMarkK)
species4=list.files(fishQLD)
full.list=unique(c(species, species2, species3, species5))

#out.dir="/home/jc246980/Species_data/All_Fish/With_data_origin/"
out.dir="/home/jc246980/Species_data/"
write.csv(full.list,paste(out.dir,'Fish_full_list_March11.csv',sep=''),row.names=F) # write out full species list to check spelling consistencies

	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		species.data.add=NULL
		
		
		if(file.exists(paste(fishatlas.dir,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishatlas.dir,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$LATITUDE)
				species.data.longdec=species.data.add$LONGITUDE
				datarecords1=cbind(species.data.latdec,species.data.longdec) 
				datarecords1$origin="NAFF"
				colnames(datarecords1)=c("Lat", "Long", "Origin")
		} 
		
				if(file.exists(paste(fishadded,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishadded,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$Lat)
				species.data.longdec=species.data.add$Long
				datarecords2=cbind(species.data.latdec,species.data.longdec) 
				datarecords2$origin="Added_refs"
				colnames(datarecords2)=c("Lat", "Long", "Origin")
				
		} 
				if(file.exists(paste(fishMarkK,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishMarkK,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$LAT)
				species.data.longdec=species.data.add$LONG
				datarecords3=cbind(species.data.latdec,species.data.longdec) 
				datarecords3$origin="MarkK"
				colnames(datarecords3)=c("Lat", "Long", "Origin")
		} 
		
				if(file.exists(paste(fishQLD,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishQLD,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$Latitude...processed)
				species.data.longdec=species.data.add$Longitude...processed
				datarecords4=cbind(species.data.latdec,species.data.longdec) 
				datarecords4$origin="QLD_museum"
				colnames(datarecords4)=c("Lat", "Long", "Origin")
		} 
		
				if(file.exists(paste(fishdata.dir,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishdata.dir,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$LATDEC)
				species.data.longdec=species.data.add$LONGDEC
				datarecords5=cbind(species.data.latdec,species.data.longdec)
				datarecords5$origin="ALA"
				colnames(datarecords5)=c("Lat", "Long", "Origin")				
				
		} 
		
				if(file.exists(paste(fishFGARA,"/", full.list[sp],sep=''))){
				species.data.add=read.csv(paste(fishFGARA,"/",full.list[sp],sep=''))
				species.data.latdec=as.data.frame(species.data.add$Latitude)
				species.data.longdec=species.data.add$Longitude
				datarecords6=cbind(species.data.latdec,species.data.longdec)
				datarecords6$origin="FGARA"
				colnames(datarecords6)=c("Lat", "Long", "Origin")				
				
		} 
		
		
		
		filenames <- c("datarecords1","datarecords2","datarecords3","datarecords4","datarecords5","datarecords6" )
		dt <- rbind.fill(mget(Filter(exists, filenames),envir = globalenv()))
		rm("datarecords1","datarecords2","datarecords3","datarecords4","datarecords5","datarecords6" )
		write.csv(dt,paste(out.dir,speciesname,".csv",sep=''),row.names=F)
		
}

		
### Now map to reach
                     
load("/home/jc246980/Hydrology.trials/Catchmentraster250.Rdata")
networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')	
out.dir="/home/jc246980/Species_data/Reach_data/"	
Fish_data = matrix(NA, nrow=nrow(networkatts), ncol=365)
Fish_data [,1] <- networkatts[,9]
speciesname=gsub('.csv','',full.list)		
colnames(Fish_data)=c("SegmentNo", speciesname)
wd="/home/jc246980/Species_data/All_Fish/";setwd(wd)
out.dir="/home/jc246980/Species_data/All_Fish/"	


	for (sp in 1:length(full.list)) { cat(full.list[sp],'\n')
		speciesname=gsub('.csv','',full.list[sp])
		dt=read.csv(paste(out.dir,"/",full.list[sp],sep=''))
		SegmentNo_SP_Present  = extract.data(cbind(dt$Long,dt$Lat),CatchmentRaster.asc) # extract Segment number from Catchment raster
		SegmentNo_SP_Present=unique(na.omit(SegmentNo_SP_Present))
		Fish_data=as.data.frame(Fish_data)
		Fish_data[(Fish_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 1
		Fish_data[!(Fish_data$SegmentNo %in% SegmentNo_SP_Present),speciesname] <- 0
		
	}

write.csv(Fish_data,paste(out.dir,'Fish_reach_Feb14.csv',sep=''),row.names=F)

###Script to merge subspecies to species and create a master list

tdata=read.csv("/home/jc246980/Species_data/Reach_data/Fish_reach_Feb14.csv")
soi=c("Mugilogobius_filifer")

temp=tdata[,grep(soi,colnames(tdata))]
temp2=tdata[,grep("SegmentNo",colnames(tdata))]
temp3=as.data.frame(cbind(temp2, temp))
temp4=temp3[which(temp3$temp2==365644),]

### Merge subspecies Melanotaenia

# temp=tdata[,intersect(grep("Melanotaenia",colnames(tdata)),grep("australis",colnames(tdata)))]
# temp$Melanotaenia_australis_master=apply(temp, 1, sum)
# temp[temp$Melanotaenia_australis_master>0,3] <-1

# tdata[,grep("Melanotaenia_australis", colnames(tdata))] = temp$Melanotaenia_australis_master # replace M. australis data with master data

# soi="Melanotaenia_splendida.australis"
# tdata=tdata[,-grep(soi,colnames(tdata)) ] # delete old sub species column

# soi = "Melanotaenia_splendida"
# temp=tdata[,grep(soi,colnames(tdata))]	
# temp$Melanotaenia_splendida_master=apply(temp, 1, sum)
# temp[temp$Melanotaenia_splendida_master>0,5] <-1

# soi="Melanotaenia_splendida"	
# tdata[,grep(soi, colnames(tdata))][1] = temp$Melanotaenia_splendida_master # replace M. splendida data with new master data with sub species merged

### Sort out spelling issues with Oxyeleotris_lineolata

temp=tdata[,intersect(grep("eleotris",colnames(tdata)),grep("lineolatus",colnames(tdata)))]

temp$Oxyeleotris_lineolata=apply(temp, 1, sum)
temp[temp$Oxyeleotris_lineolata>0,3] <-1

tdata$Oxyeleotris_lineolatus= temp$Oxyeleotris_lineolata# replace  data with master data
tdata=tdata[,-grep("Ocyeleotris_lineolatus",colnames(tdata)) ] # delete wrong named species column
tdata=tdata[,-grep("Oxyeleotris_lineolatus",colnames(tdata)) ] # delete wrong named species column

### Sort out spelling issues with Hephaestus spp

temp=tdata[,intersect(grep("Hep",colnames(tdata)),grep("epirrhinos",colnames(tdata)))]
temp$Hephaestus_epirrhinos=apply(temp, 1, sum)
temp[temp$Hephaestus_epirrhinos>0,1] <-1

tdata$Hephaestus_epirrhinos= temp$Hephaestus_epirrhinos# replace M. australis data with master data
tdata=tdata[,-grep("Hepahestus_epirrhinos",colnames(tdata)) ] # delete wrong named species column


temp=tdata[,intersect(grep("Hephae",colnames(tdata)),grep("fuliginosus",colnames(tdata)))]
temp$Hephaestus_fuliginosus=apply(temp, 1, sum)
temp[temp$Hephaestus_fuliginosus>0,1] <-1

tdata$Hephaestus_fuliginosus= temp$Hephaestus_fuliginosus# replace M. australis data with master data
tdata=tdata[,-grep("Hephaetus_fuliginosus",colnames(tdata)) ] # delete wrong named species column


out.dir="/home/jc246980/Species_data/Reach_data/"
save(tdata,file=paste(out.dir,"Fish_reach_master.Rdata",sep=''))

### remove segments with no records, exotics and estuarine species

exotics=read.csv("/home/jc246980/Species_data/Fish.exclude.csv")
exotics=exotics[,1]
### Fish
occur.file="/home/jc246980/Species_data/Reach_data/Fish_reach_master.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column

temp=occur
temp <- temp[, !(colnames(temp) %in% exotics)]
occur=temp
save(occur,file=paste(out.dir,"Fish_reach_masterV2.Rdata",sep=''))