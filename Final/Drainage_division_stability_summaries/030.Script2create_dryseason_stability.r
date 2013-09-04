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
### Determine average stability for dry season metrics for river basins
 
# load current data
cur.dir = "/home/jc246980/DrySeason/Currentdat"

load(paste(cur.dir, '/DrySeason_num.month.Rdata', sep=''))  # num.month
current_num_month=num.month[,c(31:32)]

load(paste(cur.dir, '/DrySeason_total.severity.Rdata', sep='')) # total.severity
current_total_severity=total.severity[,c(31:32)]

load(paste(cur.dir, '/DrySeason_max.clust.length.Rdata', sep='')) # max.clust.length
current_max_clust_length=max.clust.length[,c(31:32)]

load(paste(cur.dir, '/DrySeason_clust.severity.Rdata', sep=''))    # clust.severity
current_fut_clust_severity=clust.severity[,c(31:32)]

load(paste(cur.dir, '/DrySeason_month.max.clust.Rdata',sep=''))   #  month.max.clust
current_fut_month_max_clust=month.max.clust[,c(31:32)]

data.dir="/home/jc246980/DrySeason/Futuredat/Data/"
vois_data=c("max_clust_length", "fut_clust_severity","fut_month_max_clust")

	for (voi in vois_data){ cat(voi,'\n')
		
		outdelta = matrix(NA,nrow=nrow(current_fut_month_max_clust),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
		tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt
		outdelta=cbind(pos[,c(1:6)], outdelta)
		outsd = outdelta #copy the outdelta
			
		current=paste("current_", voi, sep='')
		current=get(current)
		
		for (es in ESs){ cat(es,'\n')
				
			tdata=load(paste(data.dir,es,'_',voi,'.Rdata', sep=''))
			tdata=get(tdata)
			temp=cbind(pos[,1:6],current)
			dryseason=cbind(temp,tdata)
			
			dryseason_delta=dyseason_sd=dryseason
			
			for (ii in 9:ncol(dryseason)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
				dyseason_sd[,ii] = (dryseason[,ii]-dryseason[,7])/dryseason[,8] #calculate the number of standard deviations away
			}; cat('\n')
		
			if (voi=='total_severity'| voi=='fut_clust_severity') { 
				dryseason_delta[,-c(1:8)] = dryseason_delta[,-c(1:8)]/dryseason_delta[,7] #convert to deltas as a proportion
			}else{dryseason_delta[,-c(1:8)] = dryseason_delta[,-c(1:8)]-dryseason_delta[,7]} #convert to deltas as an absolute difference
				
			for (year in YEARs){   cat(year,'\n')
							 
				cois = grep(year,colnames(dryseason_delta)) # define the year
				outquant = t(apply(as.data.frame(dryseason_delta[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
				outquant = t(apply(as.data.frame(dyseason_sd[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
				outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data
			}
				
		}
		
		write.csv(outdelta,paste(out.dir,voi,"_delta.csv",sep=''),row.names=T)	 
		write.csv(outsd,paste(out.dir,voi,"_sd.csv",sep=''),row.names=T)	 
	 
	}
 
 