################################################################################
# aggregate bioclim variables generated at 5km resolution onto Janets reaches for current (1976-2005), (1960-1990) and futures
# C. James 23rd October 2012

library(SDMTools) #load the necessary library

#### Set directories
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd)    # define and set working directory
futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' 				# define the directory with all the future data in it
gisfuture.dir = '/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/'
cur.dir='/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/current.76to05/'
cur.1960_1990.dir = '/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/baseline.61to90/bioclim/'
out.dir="/home/jc246980/Climate/5km/Current_1976_2005/Bioclim_asci_reach/"
ESs = list.files(futdir, pattern="RCP") 										# list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep=''))									# get a list of GCMs
YEAR=seq(2015, 2085, 10)														# define years of interest
tt=expand.grid('bioclim',sprintf('%02i',1:19),'.asc.gz',sep=''); Bioclims = paste(tt[,1],"_",tt[,2],tt[,3], sep='')
																	

#### get necessary files
 
baseasc = read.asc.gz('base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
pos = read.csv('base.positions.csv',as.is=TRUE)   								# import pos file
pos$UID = 1:286244 																# append unique identifier
tpos=pos	   																	# create copy of pos

#### create weights to apply 

#load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata') #load relationships table
#Reach_area = aggregate(Area_agg$Area, by = list(Area_agg$SegmentNo), sum)       
#colnames(Reach_area)=c('SegmentNo', 'Reach_area')
#Reach_area_agg<- merge(Area_agg, Reach_area, by='SegmentNo')  
#Reach_area_agg$weights =Reach_area_agg$AREA/Reach_area_agg$Reach_area 
#save(Reach_area_agg,file=("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_agg_weights.Rdata"))    

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_agg_weights.Rdata') #load relationships table with weights

#### aggregate each bioclim variable to reach for current 1976 to 2005


curdir = paste(out.dir,"current.1976to2005",sep=''); dir.create(curdir) #define and create output directory	  
						
	for(bios in Bioclims) { cat(bios,'\n') 										# cycle through each bioclim variable
			tasc = read.asc.gz(paste(cur.dir,"/",bios,sep='')) 					# bring in relevant bioclim asci
			tpos$bioclim=tasc[cbind(tpos$row,tpos$col)]     					# Append bioclim data to temp pos file
			Merged<- merge(Reach_area_agg, tpos, by='UID')        				# Merge bioclim data with Area_agg 
			Merged$weighted_bioclim=Merged$bioclim*Merged$weights
			Reach_bioclim = aggregate(Merged$weighted_bioclim, by=list(Merged$SegmentNo), sum) # Merge bioclim data for each segment number using weights (based on proportion of reach area)
			colnames(Reach_bioclim)=c('SegmentNo', 'bioclim_agg')
			Merged2<- merge(Merged, Reach_bioclim, by='SegmentNo')  			# append location info
			out = Merged2[,c(6,7, 12)] 											# append location info
			tt=gsub(".asc.gz",'',bios); colnames(out)=c("lat", "lon",tt)
			dataframe2asc(out,outdir=curdir) 									# write out the ascii grid files
			system(paste('gzip ',curdir,"/",tt,".asc",sep=''))					# gzip the data

	}

	
#### aggregate each bioclim variable to reach for current 1960 to 1990 - Rem for the TEMPERATURE variables I have left them as is (so multiplied by a factor of 10) 

out.dir="/home/jc246980/Climate/5km/Current_1960_1990/Bioclim_asci_reach/"
curdir = paste(out.dir,"current.1960to1990",sep=''); dir.create(curdir) #define and create output directory	  
						
	for(bios in Bioclims) { cat(bios,'\n') 										# cycle through each bioclim variable
			
			tasc = read.asc.gz(paste(cur.1960_1990.dir,"/",bios,sep='')) 		# bring in relevant bioclim asci
			tpos$bioclim=tasc[cbind(tpos$row,tpos$col)]     					# Append bioclim data to temp pos file
			Merged<- merge(Reach_area_agg, tpos, by='UID')        				# Merge bioclim data with Area_agg 
			Merged$weighted_bioclim=Merged$bioclim*Merged$weights
			Reach_bioclim = aggregate(Merged$weighted_bioclim, by=list(Merged$SegmentNo), sum) # Merge bioclim data for each segment number using weights (based on proportion of reach area)
			colnames(Reach_bioclim)=c('SegmentNo', 'bioclim_agg')
			Merged2<- merge(Merged, Reach_bioclim, by='SegmentNo')  			# append location info
			out = Merged2[,c(6,7, 12)] 											# append location info
			tt=gsub(".asc.gz",'',bios); colnames(out)=c("lat", "lon",tt)
			dataframe2asc(out,outdir=curdir) 									# write out the ascii grid files
			system(paste('gzip ',curdir,"/",tt,".asc",sep=''))		

	}
				   
#### aggregate each bioclim variable to reach for futures

out.dir="/home/jc246980/Climate/5km/Future/Bioclim_asci_reach/"

es=ESs[1]
gcm=GCMs[1]
year=YEAR[1]


	for(es in ESs) { cat(es,'\n') 												# cycle through each emission

		for (gcm in GCMs) { cat(gcm,'\n')									    # cycle through each GCM 
		
				for(year in YEAR) { cat(year,'\n')								# cycle through each year
						ascdir = paste(out.dir,es,"_",gcm,"_",year, sep=''); dir.create(ascdir) #define and create output directory	  
						
						for(bios in bioclims) { cat(bios,'\n') 					# cycle through each bioclim variable
								bios=Bioclims[1]
								tasc = read.asc.gz(paste(gisfuture.dir,es,"_",gcm,"_",year,"/",bios,sep='')) # bring in relevant bioclim asci
								tpos$bioclim=tasc[cbind(tpos$row,tpos$col)]     # Append bioclim data to temp pos file
								Merged<- merge(Reach_area_agg, tpos, by='UID')  # Merge bioclim data with Area_agg 
								Merged$weighted_bioclim=Merged$bioclim*Merged$weights
								Reach_bioclim = aggregate(Merged$weighted_bioclim, by=list(Merged$SegmentNo), sum) # Merge bioclim data for each segment number using weights (based on proportion of reach area)
								colnames(Reach_bioclim)=c('SegmentNo', 'bioclim_agg')
								Merged2<- merge(Merged, Reach_bioclim, by='SegmentNo')  			# append location info
								out = Merged2[,c(6,7, 12)] 											# append location info
								tt=gsub(".asc.gz",'',bios); colnames(out)=c("lat", "lon",tt)
								dataframe2asc(out,outdir=ascdir) 									# write out the ascii grid files
								system(paste('gzip ',ascdir,"/",tt,".asc",sep=''))	

						}	  
				}
		}
	}

