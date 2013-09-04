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


data.dir = "/home/jc246980/Hydrology.trials/Stability/Runoff/Data/"
wd ="/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/TomHarwood_data/" # location of current runoff
load(paste(wd, "Qrun.current_5km_means.Rdata", sep=''))
Current=as.matrix(rowSums(Qrun))

outdelta = matrix(NA,nrow=nrow(Current),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt


for (es in ESs)  { cat(es,'\n') 
				
	load(paste(data.dir,es,'_data_runoff.Rdata',sep='')) #load the data
	tdata=cbind(pos[,1:6],Current)
	data.runoff=cbind(tdata,data.runoff)
	delta.runoff=data.runoff 
	
	for (ii in 8:ncol(data.runoff)) { cat(ii,'...') #cycle through each of the gcm and year combinations		
		delta.runoff[,ii] = data.runoff[,ii]/(data.runoff[,'Current']+0.000001)#convert to deltas as a proportion
	}
					
	for (year in YEARs) { cat(year,'\n') #cycle through each of the years 
						
			cois = grep(year,colnames(delta.runoff)) #define the columns that intersect the ES & year
			outquant = t(apply(as.data.frame(delta.runoff[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
			outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
								
	}
}
		
write.csv(outdelta,paste(out.dir,"/Runoff_delta.csv",sep=''),row.names=T)	

 