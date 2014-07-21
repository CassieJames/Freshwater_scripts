####Script to aggregate species data for segment numbers where there are duplicates - this code is probably for the wet tropics work only

species.data = read.csv("/home/jc246980/Species_data/Reach_data/Fish_reach.csv") # read in species data

for(i in 2:ncol(species.data))	{cat(species.data[,i],'\n') 	
	species.data.agg = aggregate(species.data[,i], by = list(species.data$SegmentNo), sum)  
	colnames(species.data.agg) = c("SegmentNo", "species")
	if(i==2){ species.data.final=species.data.agg
	}else{species.data.final=merge(species.data.final, species.data.agg, by="SegmentNo", all.x=TRUE)
	}
	
	}
	

for(i in 2:ncol(species.data.final)) {
		temp=species.data.final[,c(1,i)]
		colnames(temp) = c("SegmentNo", "species")
		temp$species[which(temp$species>=2)] = 1				 	# Where I have aggregated segments I have generated '2' if a species is recorded in both segments - change all theses to '1'
		if(i==2){ fish.data=temp
	}else{fish.data=merge(fish.data, temp, by="SegmentNo", all.x=TRUE)
	}
	}
	
colnames(fish.data) = colnames(species.data.final)

out.dir="/home/jc246980/Freshwater_scripts/Final/Vetting/"
write.csv(fish.data,file=paste(out.dir,'Fish_agg2reach.csv',sep=''))     # save out file