###################################################################################################
# Script to prepare data for Zonation 
### C James 

library(SDMTools) #define the libraries needed
library(raster)
library(maptools)

###################################################################################################
#### Set up 250 m base file (pos250)

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)                          
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))   
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]     

####  Setting up IBRAs, extract for correct locations and appending to dataset
ibra.dir='/home/jc246980/IBRA regions/';  setwd(ibra.dir)
tasc = read.asc("ibra_250m.asc")
pos250$IBRA = extract.data(cbind(pos250$lon,pos250$lat), tasc)  	# extract data from tasc at pos250 lon and lat positions

####   Read in the river basins, extract for correct locations and append
tasc = read.asc("/home/jc246980/Zonation/river2.asc")
pos250$riverbasin  = extract.data(cbind(pos250$lon,pos250$lat), tasc)  
tdata=pos250[which(pos250$IBRA==88),]
roi = unique(na.omit(tdata$riverbasin)) 							# create a vector of all the riverbasins that transect IBRA 88 (AWT)
roi=roi[-c(1,5,10,13,14)]											# remove rivers that are not encompassed by AWT region (these are positions in the vector not riverbasin codes- beware!)
# out.dir="/home/jc246980/Zonation/"
# save(pos250,file=paste(out.dir,'Zonation_basic_info.Rdata',sep=''))	# save out file incase it crashes again

out.dir="/home/jc246980/Zonation/"
load(paste(out.dir,'Zonation_basic_info.Rdata',sep=''))

###################################################################################################
####   Create planning layer (SegmentNo.asc)

roi=c(49,58,61,62,64,71,73,77,66) 									# riverbasins of interest
tdata=pos250[which(pos250$riverbasin %in% roi),] 					# Clip dataset to riverbasins of interest (roi)
minrow=(min(tdata$row))-1; maxrow=(max(tdata$row))+1				# work out the max and min row numbers of clip and expand by 1
mincol=(min(tdata$col))-1; maxcol=(max(tdata$col))+1
tdata3=pos250[which(pos250$row>=minrow & pos250$row<=maxrow & pos250$col<=maxcol & pos250$col>=mincol),] # clip pos250 to required size

tdata3$SegmentNo[which(tdata3$row == minrow)] = -9999 				# create buffer around square with 'nodata' values
tdata3$SegmentNo[which(tdata3$row == maxrow)] = -9999
tdata3$SegmentNo[which(tdata3$col == mincol)] = -9999
tdata3$SegmentNo[which(tdata3$col == maxcol)] = -9999

tdata4=tdata3[,c("lat", "lon", "SegmentNo")]
dataframe2asc(tdata4) 												# create asc

###################################################################################################
#### Create species asc files

out.dir="/home/jc246980/Zonation/"
load(paste(out.dir,'Zonation_basic_info.Rdata',sep='')) 			# load pos250
load("/home/jc246980/Zonation/Fish_reach_aggregated.Rdata") 		# Read in species data (species.data.final)

roi=c(49,58,61,62,64,71,73,77,66) 									# riverbasins of interest
tdata=pos250[which(pos250$riverbasin %in% roi),] 					# Clip dataset to riverbasins of interest (roi)
minrow=min(tdata$row)-1; maxrow=max(tdata$row)+1					# work out the max and min row numbers of clip and expand by 1
mincol=min(tdata$col)-1; maxcol=max(tdata$col)+1
tdata3=pos250[which(pos250$row<=maxrow & pos250$row>=minrow & pos250$col<=maxcol & pos250$col>=mincol),] # clip data to size

tdata5= merge(tdata3, species.data.final, by='SegmentNo', all.x=TRUE)   # merge clipped data with species data but keep all segments where species.data does not occur

spdata=matrix(NA, nrow=nrow(tdata5), ncol=0)
spdata=cbind(spdata,tdata5[,1:7]) 

	for(i in 8:ncol(tdata5)) {
		temp=tdata5[,c(1:7,i)]
		colnames(temp) = c("SegmentNo","row", "col","lat", "lon","IBRA", "riverbasin","species")
		temp$species[which(temp$SegmentNo==-9999)] = -9999 	 		# replace all NAs in species data where Segmentno is -9999 
		temp$species[which(temp$species==0)] = -9999		 		# replace all 0's with -999
		temp$species[which(temp$species==0)] = -9999
		temp$species[which(is.na(temp$species))] = -9999				# Nas were generated with the merge command where there were Segment numbers in tdata3 that were not in the species file
		temp$species[which(temp$species==2)] = 1				 	# Where I have aggregated segments I have generated '2' if a species is recorded in both segments - change all theses to '1'
		temp$species[which(temp$row == minrow)] = -9999 	 		# replace all buffer values around square with -9999
		temp$species[which(temp$row == maxrow)] = -9999
		temp$species[which(temp$col == mincol)] = -9999
		temp$species[which(temp$col == maxcol)] = -9999
		spdata=cbind(spdata, temp[,8])
	}
	
colnames(spdata) = colnames(tdata5)	

out.dir="/home/jc246980/Zonation/"
save(spdata,file=paste(out.dir,'Zonation_Fish_AWT.Rdata',sep=''))     # save out file incase it crashes again

out.dir="/home/jc246980/Zonation/"; setwd(out.dir)
load(paste(out.dir,'Zonation_Fish_AWT.Rdata',sep=''))		# read in file

out.dir="/home/jc246980/Zonation/Species.asc/"; setwd(out.dir)
spdata_temp = spdata[,-c(1,2,3,6,7)] # create fish asc files
dataframe2asc(spdata_temp)

########################################################################################
### Create connectivity list 
### The networks are based on HydroID (stream network) but all the env info is attributed to catchment (SegmentNo) so
### for each Segment I have to read off the "nextdownID" and then look in the original database for the row where that HydroID is situated and 
### read off the segment number in column 2

networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf') # Network attributes file
tdata= networkatts[,c(2,9,13)]

### create a subset of the networkatts table with all the segment numbers that are in tdata3

tempdat=tdata[which(tdata$SegmentNo %in% tdata3$SegmentNo),]   		# Subset network attribute file to Segment numbers in clipped pos250 file (tdata3)

connect=matrix(NA, nrow=0, ncol=2)
colnames(connect)=c("SegmentNo", "t")

	for(i in 1:nrow(tempdat)) {
		temp=tempdat[i,]
		if (temp[3]==-1) {t=-1} 						  			# if next down is -1 then just return -1
		if(temp[3] %in% tempdat$HydroID){				  			# Check to see whether its in the subset list 
		t=tempdat[tempdat$HydroID==as.numeric(temp[3]),2] 			# if next down is a hydroID, need to go to row in tdata and find appropriate row and then read off second value (SegmentNo)
		}else{
		t=-1														# These are situtations which shouldn't happen but do because the rois and the networks don't exactly match!
		}
		temp2=cbind(temp[2], t)
		connect=rbind(connect,temp2)
		
	}

colnames(connect) = c("SegmentNo", "NextDown")
	
### Identify all the segments that are NOT in the networkatts table (not on a stream network)

soi = unique(tdata3$SegmentNo) 										# create list of all segments
soi_found=unique(connect$SegmentNo) 								# create a list of all segments in networkatts
missingsegs=soi[which(!(soi %in% soi_found))]   					# identify those that are missing
missingsegs=as.data.frame(missingsegs)
missingsegs$NextDown = -1
colnames(missingsegs)[1]="SegmentNo"
connect2=rbind(connect, missingsegs)	

### Where Segment number is duplicated in connection file make the downstream segment the same number

dup_segments=connect2$SegmentNo[duplicated(connect2$SegmentNo)] 
dup_segments=unique(dup_segments) # must be some triplicates too!
	
	for(ii in 1:length(dup_segments)) {
		dup_segs_data=connect2[connect2$SegmentNo == dup_segments[ii],]
		newseg=cbind(dup_segs_data[[1]][1], dup_segs_data[[2]][1])
		if(ii==1){tdata=newseg
		}else{tdata=rbind(tdata, newseg)
		}
	}
colnames(tdata)= c("SegmentNo", "NextDown")
	
### Now remove the duplicated segments from the orginal connection file and bind the new connect file for duplicate segments

tdata=as.data.frame(tdata)
connect3=connect2[which(!(connect2$SegmentNo %in% tdata$SegmentNo)),]
connect3=rbind(connect3, tdata)
	
out.dir="/home/jc246980/Zonation/"
save(connect3,file=paste(out.dir,'Zonation_connectivity_AWT.Rdata',sep=''))     # save out file incase it crashes again

load(paste(out.dir,'Zonation_connectivity_AWT.Rdata',sep=''))   
write.csv(connect3,paste(out.dir,"Connections.csv",sep=''),row.names=F)		 

###################################################################################################
### Create mask for analysis that limits extent to roi

out.dir="/home/jc246980/Zonation/"; setwd(out.dir)
load(paste(out.dir,'Zonation_basic_info.Rdata',sep=''))
roi=read.dbf("/home/jc246980/Zonation/ROI.dbf")# riverbasins of interest (determined via manual selection in arcgis using lasoo feature selection and saving as a dataframe)
								
tdata=pos250[which(pos250$SegmentNo %in% roi$SegmentNo),] 					# Clip dataset to riverbasins of interest (roi)
minrow=min(tdata$row)-1; maxrow=max(tdata$row)+1					# work out the max and min row numbers of clip and expand by 1
mincol=min(tdata$col)-1; maxcol=max(tdata$col)+1
tdata3=pos250[which(pos250$row<=maxrow & pos250$row>=minrow & pos250$col<=maxcol & pos250$col>=mincol),] # clip data to size

RB=tdata3[, c(3,4,5)]
colnames(RB)[3] = "mask"
RB$mask[which(!(RB$mask %in% roi$SegmentNo))] = -9999	
RB$mask[which(RB$mask %in% roi$SegmentNo)] = 1	
dataframe2asc(RB)



