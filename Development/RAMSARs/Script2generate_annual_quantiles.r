######################################################################W############################
#Script to generate annual deltas and sd and calculate quantiles for tmp and pre for mapping of RAMSARS
#C. James..........................................7th November 2012

###Load necessary libraries
library(SDMTools)

###Set up directories

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_site_data/tmp/"
out.dir='/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/'

###Set up base files

base.asc = read.asc.gz('base.asc.gz');
baseasc=base.asc
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
VOIS = c('pre')                                                                              			 #define the variable of interest
YEARs = seq(2015, 2085,10)     


# Calculate annual percentile, SD and delta's for data annual mean temperature
	
                                                                            		 #define the year of interest

	
	for (voi in VOIS) { cat(voi,'\n') 
		
		load(paste('/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/',voi,'.Rdata',sep='')) 		#load the annual temperature data
		
		for (year in YEARs) { cat(year,'\n') 
			
			tdata = as.matrix(annualdata[,grep(year,colnames(annualdata))])                         			#subset the data by year 2085
			annualdelta = annualsd = tdata                                                           			#copy annual data to be replaced with number of standard deviation and percentiles

			for (ii in 1:ncol(tdata)) { cat(ii,'...')                                                 			#cycle through each column to calculate the num sd away from current
					  annualsd[,ii] = (tdata[,ii]-annualdata[,1])/annualdata[,2] 
					}; cat('\n')

			
			if (voi=="tmp") {annualdelta[,] = tdata - annualdata[,1] 											#If voi is temperature calculate delta as absolute difference
			}else{annualdelta[,]=tdata/annualdata[,1]+0.000001}													#If voi is preciptation calculate delta as proportional difference
						
			outdelta = matrix(NA,nrow=nrow(tdata),ncol=6);                                                		#define the output matrix
			colnames(outdelta) = c('Delta10', 'Delta50', 'Delta90', 'SD10', 'SD50', 'SD90')               		#add the column names
			
			for (es in ESs) { cat(es,'\n') 
				
				cois = grep(es,colnames(annualdelta))                                                    		#define the columns for RCP85
				outquant = t(apply(annualdelta[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outdelta[,1:3] = outquant[,]                                                                  	#copy out the data
				outquant = t(apply(annualsd[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outdelta[,4:6] = outquant[,]
				save(outdelta,file=paste(out.dir,voi,"/",es,"_", year,'_percentiles.Rdata',sep=''))

		   }
	   }
   }
	
	
	
	
	
	
	
	