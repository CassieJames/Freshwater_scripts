###################################################################################################
####Script to aggregate species data for segment numbers where there are duplicates
#### C James 
species.data = read.csv("/home/jc246980/Species_data/Reach_data/Crayfish_reach.csv") # read in species data

	for(i in 2:11)	{	
		species.data.agg = aggregate(species.data[,i], by = list(species.data$SegmentNo), sum)  
		colnames(species.data.agg) = c("SegmentNo", "species")
		if(i==2){ species.data.final=species.data.agg
		}else{species.data.final=merge(species.data.final, species.data.agg, by="SegmentNo", all.x=TRUE)
		}
		
	}

tt=colnames(species.data[,1:11])	
colnames(species.data.final) = tt

out.dir="/home/jc246980/Zonation/"
save(species.data.final,file=paste(out.dir,'Fish_reach_aggregated.Rdata',sep=''))     # save