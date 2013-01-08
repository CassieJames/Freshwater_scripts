################################################################################
# Script to aggregate dry season severity variables generated at 5km resolution onto Janets reaches for futures
# C. James 20th November 2012

library(SDMTools) #load the necessary library

###Set directories

futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/'	
data.dir="/home/jc246980/DrySeason/Futuredat/Data/"
out.dir="/home/jc246980/DrySeason/DrySeason_reach/"

ESs = list.files(futdir, pattern="RCP") 										# list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep=''))									# get a list of GCMs
YEAR=seq(2015, 2085, 10)
VOIS=c("num_month", "total_severity", "max_clust_length","fut_clust_severity", "fut_month_max_clust")

###Get necessary files
 
baseasc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)   								# import pos file
pos$UID = 1:286244 																# append unique identifier
tpos=pos	
DRYSEASON=NULL

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_agg_weights.Rdata') #load relationships table with weights


	for(voi in VOIS) { cat(voi,'\n') 	
		
		for(es in ESs) { cat(es,'\n') 											# cycle through each emission
					
			tdata=load(paste(data.dir,es,"_",voi,".Rdata", sep='')) 			# load data for each varable and each es
			tdata=get(tdata)
			
			for(gcm in GCMs) {
			
				cois=NULL
				cois = c(cois,grep(gcm,colnames(tdata))) 
				gcmoi = tdata[,cois]	# subset data by gcm				
				gcm_location=cbind(tpos, gcmoi)	# append location information and identifier - not generated as part of the dry season myfun scripts or JV's orginal precip data
				
				for (i in 6:ncol(gcm_location)) {
				
					Merged<- merge(Reach_area_agg, gcm_location[,c(5,i)], by='UID')         # Merge Area_agg with 5km pos file                            
					Merged$weighted_data=Merged[,6]*Merged$weights
					Reach_dryseason= aggregate(Merged$weighted_data, by=list(Merged$SegmentNo), sum) # Merge bioclim data for each segment number using weights (based on proportion of reach area)
					
					colnames(Reach_dryseason)[1]="SegmentNo"
					
					if (i==6) {Dry_metrics=Reach_dryseason	
					}else{Dry_metrics=merge(Dry_metrics, Reach_dryseason, by="SegmentNo")} # aggregate seems to reorder things so its important to merge!
					
					if(voi=="total_severity"|voi== "fut_clust_severity"){Dry_metrics=round(Dry_metrics,4)
					}else{Dry_metrics=round(Dry_metrics,0)}
			
				}
			
			tt= c("SegmentNo", YEAR)
			colnames(Dry_metrics)=tt
			write.csv(Dry_metrics,paste(out.dir,es,"_",gcm,"_",voi,".csv",sep=''),row.names=F)	
			
			}

		}
		
	}	      