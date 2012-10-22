################################################################################
# aggregate bioclim variables generated at 5km resolution onto Janets reaches

library(SDMTools) #load the necessary library

#### Set directories
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd)    # define and set working directory
futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' 				# define the directory with all the future data in it
gisfuture.dir = '/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/'
out.dir="/home/jc246980/Climate/5km/Current_1976_2005/Bioclim_asci_reach/"
ESs = list.files(futdir, pattern="RCP") 										# list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep=''))									# get a list of GCMs
YEAR=seq(2015, 2085, 10)														# define years of interest
Bioclims=expand.grid('bioclim_',sprintf('%02i',1:19),'.asc.gz',sep='')
																	# create copy of pos

#### get necessary files
 
baseasc = read.asc.gz('base.asc.gz')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier
pos = read.csv('base.positions.csv',as.is=TRUE)   								# import pos file
pos$UID = 1:286244 																# append unique identifier
tpos=pos	   
load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata') #load relationships table
                       

#### aggregate each bioclim variable to reach

es=ESs[1]
gcm=GCMs[1]
year=YEAR[1]


	for(es in ESs) { cat(es,'\n') 												# cycle through each emission

		for (gcm in GCMs) { cat(gcm,'\n')									    # cycle through each GCM 
		
				for(year in YEAR) { cat(year,'\n')								# cycle through each year
						ascdir = paste(out.dir,es,"_",gcm,"_",year,"/", sep=''); dir.create(ascdir) #define and create output directory	  
						
						for(bios in bioclims) { cat(bios,'\n') 					# cycle through each bioclim variable
								tasc = read.asc.gz(paste(gisfuture.dir,es,"_",gcm,"_",year,"/",bios,sep='')) # bring in relevant bioclim asci
								tpos$bioclim=tasc[cbind(tpos$row,tpos$col)]     # Append bioclim data to temp pos file
								Merged<- merge(Area_agg, tpos, by='UID')    
								Reach_bioclim = aggregate(Merged$bioclim, by = list(FINAL$REACHID), mean) 
								out = cbind(tpos[,1:2],Reach_bioclim) #append location info
								dataframe2asc(out,outdir=ascdir) #write out the ascii grid files
								system(paste('gzip ',ascdir,'*.asc',sep='')) #gzip the data

						}	  
				}
		}
	}

