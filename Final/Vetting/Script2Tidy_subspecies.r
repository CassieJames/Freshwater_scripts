###################################################################################################
#### Script to merge subspecies to species and create a master list

### Fish

tdata=read.csv("/home/jc246980/Species_data/Reach_data/Fish_reach.csv")

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


temp=tdata[,intersect(grep("eleotris",colnames(tdata)),grep("lineolatus",colnames(tdata)))]

temp$Oxyeleotris_lineolata=apply(temp, 1, sum)
temp[temp$Oxyeleotris_lineolata>0,3] <-1

tdata$Oxyeleotris_lineolata= temp$Oxyeleotris_lineolata# replace M. australis data with master data
tdata=tdata[,-grep("Ocyeleotris_lineolatus",colnames(tdata)) ] # delete wrong named species column
tdata=tdata[,-grep("Oxyeleotris_lineolatus",colnames(tdata)) ] # delete wrong named species column


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
write.csv(tdata,paste(out.dir,"Fish_reach_master.csv",sep=''),row.names=FALSE)	 
save(tdata,file=paste(out.dir,"Fish_reach_master_V2.Rdata",sep=''))



### Turtles

tdata=read.csv("/home/jc246980/Species_data/Reach_data/Turtles_reach.csv")

temp=tdata[,intersect(grep("Emydura",colnames(tdata)),grep("macquarii",colnames(tdata)))]
temp$Emydura_macquarii_master=apply(temp, 1, sum)
temp[temp$Emydura_macquarii_master>0,6] <-1

soi="Emydura_macquarii"
tdata[,grep(soi, colnames(tdata))] [1]= temp$Emydura_macquarii_master # replace M. australis data with master data

temp=tdata[,intersect(grep("Emydura",colnames(tdata)),grep("subglobosa",colnames(tdata)))]
temp$Emydura_subglobosa_master=apply(temp, 1, sum)
temp[temp$Emydura_subglobosa_master>0,4] <-1

tdata[,grep("Emydura_subglobosa", colnames(tdata))] [1]= temp$Emydura_subglobosa_master # replace M. australis data with master data

out.dir="/home/jc246980/Species_data/Reach_data/"
write.csv(tdata,paste(out.dir,"Turtles_reach_master.csv",sep=''),row.names=FALSE)	 
save(tdata,file=paste(out.dir,"Turtles_reach_master.Rdata",sep=''))

###################################################################################################
### Crayfish Script to merge subspecies with species and sort out a few spelling errors that have resulted in duplicates

tdata=read.csv("/home/jc246980/Species_data/Reach_data/Crayfish_reach.csv")

temp=tdata[,intersect(grep("Cherax",colnames(tdata)),grep("destructor",colnames(tdata)))]
temp$Cherax_destructor_master=apply(temp, 1, sum)
temp[temp$Cherax_destructor_master>0,4] <-1

tdata[,grep("Cherax_destructor", colnames(tdata))] [1]= temp$Cherax_destructor_master # replace M. australis data with master data

temp=tdata[,intersect(grep("Euastacus",colnames(tdata)),grep("miran",colnames(tdata)))]
temp$Euastacus_mirangudjin_master=apply(temp, 1, sum)
temp[temp$Euastacus_mirangudjin_master>0,3] <-1

tdata[,grep("Euastacus_mirangudjin", colnames(tdata))] = temp$Euastacus_mirangudjin_master # replace M. australis data with master data
tdata=tdata[,-grep("Euastacus_mirangudgin",colnames(tdata)) ] # delete wrong named species column

temp=tdata[,intersect(grep("Euastacus",colnames(tdata)),grep("robert",colnames(tdata)))]
temp$Euastacus_robertsi_master=apply(temp, 1, sum)
temp[temp$Euastacus_robertsi_master>0,3] <-1

tdata[,grep("Euastacus_robertsi", colnames(tdata))] = temp$Euastacus_robertsi_master # replace M. australis data with master data
tdata=tdata[,-grep("Euastacus_robertisi",colnames(tdata)) ] # delete wrong named species column



out.dir="/home/jc246980/Species_data/Reach_data/"
write.csv(tdata,paste(out.dir,"Crayfish_reach_master.csv",sep=''),row.names=FALSE)	
save(tdata,file=paste(out.dir,"Crayfish_reach_master.Rdata",sep=''))

###################################################################################################
####Script to further tidy and remove exotics

exotics=c("Amphilophus_labiatus", "Carassius_auratus", "Gambusia_affinis","Gambusia_holbrooki","Hemichromis_bimaculatus","Oreochromis_mossambicus","Perca_fluviatilis","Poecilia_reticulata","Puntius_conchonius","Salmo_salar","Salmo_trutta","Salvelinus_fontinalis","Tilapia_mariae","Xiphophorus_helleri","Xiphophorus_maculatus")

### Fish
occur.file="/home/jc246980/Species_data/Reach_data/Fish_reach_master.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column

temp=occur
for (ex in exotics) {temp=temp[,-grep(ex,colnames(temp))]}
occur=temp

save(occur,file=paste(out.dir,"Fish_reach_master_V2.Rdata",sep=''))

### Turtles


occur.file="/home/jc246980/Species_data/Reach_data/Turtles_reach_master.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
save(occur,file=paste(out.dir,"Turtles_reach_master_V2.Rdata",sep=''))


### Crayfish

out.dir="/home/jc246980/Species_data/Reach_data/"
occur.file="/home/jc246980/Species_data/Reach_data/Crayfish_reach.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
save(occur,file=paste(out.dir,"Crayfish_reach_master.Rdata",sep=''))



### Frog


occur.file="/home/jc246980/Species_data/Reach_data/Frog_reach.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
save(occur,file=paste(out.dir,"Frog_reach_master.Rdata",sep=''))






