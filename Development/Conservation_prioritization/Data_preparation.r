###################################################################################################
# Script to prepare data for Zonation 
### C James 

library(SDMTools) #define the libraries needed
library(raster)
library(maptools)

#### Set up 250 m raster grid with river segment identifier appended  

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                          
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))   
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]     


####  Setting up IBRAs, extract for correct locations and appending to dataset

ibra.dir='/home/jc246980/IBRA regions/';  setwd(ibra.dir)
tasc = read.asc("ibra_250m.asc")
pos250$IBRA = extract.data(cbind(pos250$lon,pos250$lat), tasc)  # extract data from tasc at pos250 lon and lat positions


####   Read in the river basins, extract for correct locations and append

tasc = read.asc("/home/jc246980/Zonation/river2.asc")
pos250$riverbasin  = extract.data(cbind(pos250$lon,pos250$lat), tasc)  
tdata=pos250[which(pos250$IBRA==88),]
roi = unique(na.omit(tdata$riverbasin)) # create a vector of all the riverbasins that transect IBRA 88 (AWT)
roi=roi[-c(1,5,10,13,14)]# remove rivers that are not encompassed by AWT region (these are positions in the vector not riverbasin codes- beware!)

out.dir="/home/jc246980/Zonation/"
save(pos250,file=paste(out.dir,'Zonation_basic_info.Rdata',sep=''))     # save out file incase it crashes again

out.dir="/home/jc246980/Zonation/"
load(paste(out.dir,'Zonation_basic_info.Rdata',sep=''))

####   Clip data to roi with a 1 unit buffer

tdata=pos250[which(pos250$riverbasin %in% roi),] # Clip dataset to riverbasins of interest (roi)

minrow=(min(tdata$row))-1; maxrow=(max(tdata$row))+1# work out the max and min row numbers of clip and expand by 1
mincol=(min(tdata$col))-1; maxcol=(max(tdata$col))-1

tdata3=pos250[which(pos250$row<=maxrow & pos250$row>=minrow & pos250$col<=maxcol & pos250$col>=mincol),] # clip data to size

# tdata3$SegmentNo[which(tdata3$row == minrow)] = -9999
# tdata3$SegmentNo[which(tdata3$row == maxrow)] = -9999
# tdata3$SegmentNo[which(tdata3$col == mincol)] = -9999
# tdata3$SegmentNo[which(tdata3$col == maxcol)] = -9999

out.dir="/home/jc246980/Zonation/"; setwd(out.dir)
tdata4 = tdata3[,c("lat", "lon","SegmentNo")] # save out planning unit file (SegmentNo) 
dataframe2asc(tdata4)

#### Attribute species info

tdata=pos250[which(pos250$riverbasin %in% roi),] # Clip dataset to riverbasins of interest (roi)
minrow=min(tdata$row)-1; maxrow=max(tdata$row)+1# work out the max and min row numbers of clip and expand by 1
mincol=min(tdata$col)-1; maxcol=max(tdata$col)+1
tdata3=pos250[which(pos250$row<=maxrow & pos250$row>=minrow & pos250$col<=maxcol & pos250$col>=mincol),] # clip data to size

species.data = read.csv("/home/jc246980/Species_data/Reach_data/Fish_reach.csv") # read in species data
tdata3= merge(tdata3, species.data, by='SegmentNo')   # merge with species data


spdata=matrix(NA, nrow=nrow(tdata3), ncol=0)
spdata=cbind(spdata,tdata3[,1]) 

	for(i in 8:ncol(spdata)) {
		temp=spdata[,c(1:7,i)]
		colnames(temp) = c("SegmentNo","row", "col","lat", "lon","IBRA", "riverbasin","species")
		temp$species[which(temp$species==0)] = -9999 # replace all 0's with -999	
		temp$species[which(temp$row == minrow)] = -9999
		temp$species[which(temp$row == maxrow)] = -9999
		temp$species[which(temp$col == mincol)] = -9999
		temp$species[which(temp$col == maxcol)] = -9999
		tdata=merge(spdata, temp, by="SegmentNo")
	}
	
colnames(tdata) = colnames(tdata3)	

out.dir="/home/jc246980/Zonation/"
save(spdata,file=paste(out.dir,'Zonation_Fish_AWT.Rdata',sep=''))     # save out file incase it crashes again

out.dir="/home/jc246980/Zonation/"; setwd(out.dir)
load(paste(out.dir,'Zonation_Fish_AWT.Rdata',sep=''))		# read in file

spdata_temp = spdata[,c("lat", "lon","Acanthopagrus_australis")] # save out fish file to check
dataframe2asc(spdata_temp)




#########################################################################################
###Create connectivity list (the networks are based on HydroID (stream network) but all the env info is attributed to catchment (SegmentNo) so
### for each Segment I have to read off the "nextdownID" and then look in the original database for the row where that HydroID is situated and 
### read off the segment number

networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')

tdata= networkatts[,c(2,9,13)]
connect=matrix(NA, nrow=0, ncol=2)
colnames(connect)=c("SegmentNo", "t")


	for(i in 1:nrow(tdata)) {
		temp=tdata[i,]
		if (temp[3]==-1) {t=-1 # if next down is -1 then just return -1
		}else{ 
		t=tdata[tdata$HydroID==as.numeric(temp[3]),2] # if next down is a hydroID, need to go to row in tdata and find appropriate row and then read off second value (SegmentNo)
		}
		temp2=cbind(temp[2], t)
		connect=rbind(connect,temp2)
		
	}

out.dir="/home/jc246980/Zonation/"
save(connect,file=paste(out.dir,'Zonation_connectivity_ALL.Rdata',sep=''))     # save out file incase it crashes again




