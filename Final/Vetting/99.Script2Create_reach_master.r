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
occur.file="/home/jc246980/Species_data/Reach_data/Crayfish_reach.Rdata" #give the full file path of your species data - looks like master is not used and subspecies already tidy
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



