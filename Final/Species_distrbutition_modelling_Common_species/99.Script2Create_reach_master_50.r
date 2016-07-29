###################################################################################################
####Script to tidy and remove exotics

out.dir="/home/jc246980/Species_data/Reach_data/"

### Fish
exotics=read.csv("/home/jc246980/SDM/fish.to.exclude.csv")
exotics <- exotics[which(exotics[,"action"] =="exclude"),1]

occur.file="/home/jc246980/Species_data/Reach_data/Fish_reach_master.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
occur=as.data.frame(t(occur))
occur$count =rowSums(occur[,1:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>49),] #remove species that are present in less than 50 segments
occur=as.data.frame(t(occur))
temp=occur
temp <- temp[, !(colnames(temp) %in% exotics)]
occur=temp
save(occur,file=paste(out.dir,"Fish_reach_master_50.Rdata",sep=''))


### Turtles

occur=read.csv(paste(out.dir,'Turtles_reach.csv',sep=''))
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
occur=as.data.frame(t(occur))
occur$count =rowSums(occur[,1:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>49),] #remove species that are present in less than 50 segments
occur=as.data.frame(t(occur))
save(occur,file=paste(out.dir,"Turtles_reach_master_50.Rdata",sep=''))



### Crayfish

out.dir="/home/jc246980/Species_data/Reach_data/"
occur.file="/home/jc246980/Species_data/Reach_data/Crayfish_reach.Rdata" #give the full file path of your species data - looks like master is not used and subspecies already tidy
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
occur=as.data.frame(t(occur))
occur$count =rowSums(occur[,1:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>49),] #remove species that are present in less than 50 segments
occur=as.data.frame(t(occur))
save(occur,file=paste(out.dir,"Crayfish_reach_master_50.Rdata",sep=''))


### Frog


occur.file="/home/jc246980/Species_data/Reach_data/Frog_reach.Rdata" #give the full file path of your species data
occur=load(occur.file)
occur=get(occur) #rename species occurrence data to 'occur'
occur$count=rowSums(occur[,2:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>0),] #remove SegmentNos (rows) with no occurrence records for any species
occur=occur[,-grep('count',colnames(occur))]  #remove the 'count' column
occur=as.data.frame(t(occur))
occur$count =rowSums(occur[,1:ncol(occur)]) #count all presence records for each SegmentNo
occur=occur[which(occur$count>49),] #remove species that are present in less than 50 segments
occur=as.data.frame(t(occur))
save(occur,file=paste(out.dir,"Frogs_reach_master_50.Rdata",sep=''))



