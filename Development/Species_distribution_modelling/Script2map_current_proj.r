###################################################################################################
### Script to map current projections
### C James

library(SDMTools)
wd="/home/jc246980/SDM/"
load(paste(wd,"data_trial.Rdata",sep=''))
load("/home/jc246980/Zonation/Fish_reach_aggregated.Rdata") 	

image.dir = "/home/jc246980/SDM/Images/Current_projections/"
data.dir = "/home/jc148322/NARPfreshwater/SDM/models/"
image.data.dir="/home/jc246980/SDM/Images/Image_data/"
all.cols = colorRampPalette(c("tan","forestgreen"))(10)

### read in catchment asc at 250 m resolution, create pos250 and append segmentno info
wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   
CatchmentRaster.asc = read.asc(paste(wd,'catchmentraster250m2.asc', sep=''))    # read in the catchment ascii grid file at 250m resolution
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))         # convert asci to position file at 250m resolution 
pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]                     # extract and append lats and longs to 250m position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 
pos250$SegmentNo = CatchmentRaster.asc[cbind(pos250$row,pos250$col)]  

species = list.files(data.dir)


pos250_sp = merge(pos250, species.data.final[,c("SegmentNo", "Galaxias_brevipinnis")], by="SegmentNo", all.x=TRUE)
longs=pos250_sp$lon[which(pos250_sp$Galaxias_brevipinnis ==1)]
lats=pos250_sp$lat[which(pos250_sp$Galaxias_brevipinnis ==1)]


	for (i in 1:length(species)) {
		
		tdata=read.csv(paste(data.dir, species[i],"/output/projection/current.csv", sep=''))
		colnames(tdata)[1] ="SegmentNo"
		species.map=merge(pos250, tdata[,c("SegmentNo", , by="SegmentNo",all.x=TRUE)
		save(species.map,file=paste(image.data.dir,species[i],"_cur_proj.Rdata",sep='')) # Save out merged files incase figures need to be done.
		
		xlim=c(min(species.map$lon),max(species.map$lon))
		ylim=c(min(species.map$lat.x),max(species.map$lat.x))

		setwd(image.dir)
		png(paste('trial_proj_current.png',sep=''),width=7, height=7, units='cm',res=300, pointsize=5, bg='white')
		tasc = CatchmentRaster.asc; tasc[cbind(species.map$row,species.map$col)] = species.map[,7]
		image(tasc, ann=FALSE,axes=FALSE,col=all.cols, xlim=xlim,ylim=ylim,xpd=FALSE)  
		
		species.occurs = pos250_sp[, species[i]]
		longs=species.occurs$lon[which(species.occurs$species ==1)]
		lats=species.occurs$lat[which(pos250_sp$species ==1)]
		
		points(longs,lats, xlim=xlim,ylim=ylim, col='red', pch=20, add=TRUE)
		
		dev.off() #close out the image
	}