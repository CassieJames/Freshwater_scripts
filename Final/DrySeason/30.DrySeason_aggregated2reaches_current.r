################################################################################
# Script to aggregate dry season severity variables generated at 5km resolution onto Janets reaches for currents
# C. James 20th November 2012

library(SDMTools) #load the necessary library

###Set directories

data.dir="/home/jc246980/DrySeason/Currentdat/"
out.dir="/home/jc246980/DrySeason/DrySeason_reach/"
VOIS=c("num.month", "total.severity", "max.clust.length","clust.severity", "month.max.clust")

###Get necessary files
 
baseasc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)   								# import pos file
pos$UID = 1:286244 																# append unique identifier
tpos=pos	

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_agg_weights.Rdata') #load relationships table with weights

	for(voi in VOIS) { cat(voi,'\n') 	
					
			tdata=load(paste(data.dir,"DrySeason_",voi,".Rdata", sep='')) 			# load data for each varable
			tdata=get(tdata)
			tempdat=cbind(tpos, tdata[,paste(voi,".mean", sep='')])	# append location information and identifier - not generated as part of the dry season myfun scripts or JV's orginal precip data
			Merged<- merge(Reach_area_agg, tempdat, by='UID')       # Merge Area_agg with 5km pos file                            
			Merged$weighted_data=Merged[,10]*Merged$weights			# Apply weights
			Dry_metrics= aggregate(Merged$weighted_data, by=list(Merged$SegmentNo), sum) # Merge bioclim data for each segment number using weights (based on proportion of reach area occupied)
					
			if(voi=="total_severity"|voi== "clust_severity"){Dry_metrics=round(Dry_metrics,4) # round as appropriate
			}else{Dry_metrics=round(Dry_metrics,0)}
			
			tt= c("SegmentNo", voi)
			colnames(Dry_metrics)=tt
			write.csv(Dry_metrics,paste(out.dir,"Current_",voi,".csv",sep=''),row.names=F)	
	}
	      