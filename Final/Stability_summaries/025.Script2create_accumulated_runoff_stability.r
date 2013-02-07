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
out.dir="/home/jc246980/Hydrology.trials/Accumulated_reach/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085, 10)

###################################################################################################
### Calculate deltas for acucmulated runoff

wd ="/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/" # location of current runoff
current=read.csv(paste(wd, "Current_static.csv", sep=''))
current$annualtotal=rowSums(current[,c(2:13)])

delta = matrix(NA,nrow=nrow(current),ncol=19); #define the output matrix
colnames(delta) = c("SegmentNo",GCMs)

outdelta = matrix(NA,nrow=nrow(current),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt

	for (es in ESs)  { cat(es,'\n') 
		
		for (yy in YEARs) {			
			for (gcm in GCMs) {
			
			tdata=read.csv(paste(wd,es,"_", gcm,".", yy,'.csv',sep='')) #load the data
			tdata$annualtotal_fut=rowSums(tdata[,c(2:13)])
			tdata=merge(current[,c("annualtotal", "SegmentNo")], tdata[,c("SegmentNo","annualtotal_fut")], by="SegmentNo")
			tdata$delta = (tdata[,"annualtotal_fut"]+0.000001)/(tdata[,'annualtotal']+0.000001)#convert to deltas as a proportion
			delta[,"SegmentNo"] = tdata$SegmentNo #copy out the data
			delta[,grep(gcm,colnames(delta))] = tdata$delta #copy out the data
			
			}
			
			outquant = t(apply(delta,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
			outdelta[,intersect(grep(yy,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
			
		}

	}
	
	outdelta=cbind(delta[,"SegmentNo"], outdelta)
	colnames(outdelta)[1]="SegmentNo"
	write.csv(outdelta,paste(out.dir,"Accumulated_runoff_delta.csv",sep=''),row.names=T)	



# ###Load river and ramsar info

# RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
# pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)      # Map river basins onto postition file 
# RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

# Ramsar.asc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load river basin asc
# pos$ramsar  = extract.data(cbind(pos$lon,pos$lat), Ramsar.asc)      # Map river basins onto postition file 
# RAMSARS = unique(na.omit(pos$ramsar)) # create river basin vector 

# out.dir="/home/jc246980/Stability/Output/"















