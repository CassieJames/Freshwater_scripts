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

load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") 

dryseason=matrix(NA, nrow=1466889, ncol=1)
dryseason[,1]=1:1466889; colnames(dryseason)[1]=c("SegmentNo")


	for(voi in VOIS) { cat(voi,'\n') 	
		
		for(es in ESs) { cat(es,'\n') 											# cycle through each emission
					
			tdata=load(paste(data.dir,es,"_",voi,".Rdata", sep='')) 			# load data for each varable and each es
			
			For(gcm in GCMs) {
			
				cois=NULL
				cois = c(cois,grep(gcm,colnames(tdata))) 						# subset data by gcm
				gcmoi = tdata[,cois]					
				gcm_location=cbind(tpos, gcmoi)									# append location information and identifier
				
				for (i in 5:ncol(gcm_location) {
				
					FINAL<- merge(Area_agg, gcm_location[,i], by='UID')         # Merge Area_agg with 5km pos file                            
					agg_data = aggregate(FINAL$Runoff, by = list(FINAL$SegmentNo), mean) 	  # Aggregate runoff to reaches 
					Dry_metrics=cbind(dryseason, agg_data$x)
					Dry_metrics=round(Dry_metrics,4)
			
				}
			
			tt= c("SegmentNo", YEAR)
			colnames(Dry_metrics)=tt
			write.csv(Dry_metrics,paste(out.dir,es,"_",gcm,".csv",sep=''),row.names=F)	
			
			}

		}
		
	}	      