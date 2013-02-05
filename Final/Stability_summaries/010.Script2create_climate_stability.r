######################################################################W############################
#Script to determine stabilities for key river basins across Australia
#C. James..........................................14th January 2013

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
out.dir="/home/jc246980/Stability/Output/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085, 10)

###Load river and ramsar info

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)      # Map river basins onto postition file 
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

Ramsar.asc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load river basin asc
pos$ramsar  = extract.data(cbind(pos$lon,pos$lat), Ramsar.asc)      # Map river basins onto postition file 
RAMSARS = unique(na.omit(pos$ramsar)) # create river basin vector

###################################################################################################
####determine average stability for temperature mean, max, min, and precipitation

data.dir ="/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/"
vois=c("tmp", "tmx","tmn", "pre")

	for (voi in vois) { cat(voi,'\n') #cycle through each variable of interest
		load(paste(data.dir,voi,'.Rdata',sep='')) #load the data
		annualdata=cbind(pos[,1:6],annualdata)
		annualsd=annualdelta=annualdata
		
		outdelta = matrix(NA,nrow=nrow(annualdata),ncol=(3*length(ESs)*length(YEARs))); #define the output matrix
		tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt	
		outdelta=cbind(pos[,c(1:6)], outdelta)
		outsd = outdelta #copy the outdelta
		
		for (ii in 9:584) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
			annualsd[,ii] = (annualdata[,ii]-annualdata[,7])/annualdata[,8] #calculate the number of standard deviations away
		}; cat('\n')
		
		if (voi=='pre') { 
			annualdelta[,-c(1:8)] = annualdelta[,-c(1:8)]/annualdelta[,7] #convert to deltas as a proportion
		}else{annualdelta[,-c(1:8)] = annualdelta[,-c(1:8)]-annualdelta[,7]} #convert to deltas as an absolute difference
				
				
	###now create and save the percentiles
				
		for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse
					
			for (year in YEARs) { cat(year,'\n') #cycle through each of the years 
						
				cois = intersect(grep(year,colnames(annualdelta)),grep(es,colnames(annualdelta))) #define the columns that intersect the ES & year
				outquant = t(apply(as.data.frame(annualdelta[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
				outquant = t(apply(as.data.frame(annualsd[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data
					
				}
			}
		
		write.csv(outdelta,paste(out.dir,voi,"_delta.csv",sep=''),row.names=T)	 
		write.csv(outsd,paste(out.dir,voi,"_sd.csv",sep=''),row.names=T)			
	}
		