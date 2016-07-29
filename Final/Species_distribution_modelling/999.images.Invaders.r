#Climate change biology paper - new figure September 9th 2015

# C James 

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos

image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
cols = all.cols[11:1] # blue to red		


PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

##### create background

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
	tascCUR=make.asc(pos[,'Current'])


##### Create plots


	png(paste(es,'_losses','_',yr,'.Nov2015.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters
	
	taxa=c('fish','crayfish','turtles','frog')
	
	for (tax in taxa) {
		
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)	
	if (tax==c("fish"))	zlim=c(1,36)
	if (tax==c("crayfish"))	zlim=c(1,8)
	if (tax==c("turtles"))	zlim=c(1,6)
	if (tax==c("frog"))	zlim=c(1,13)

	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	#text(130,-39.5,"Gains",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)
	
	}
	
	png(paste(es,'_gains','_',yr,'.Nov2015.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters
	
	taxa=c('fish','crayfish','turtles','frog')
	
	for (tax in taxa) {
		
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	if (tax==c("fish"))	zlim=c(1,36)
	if (tax==c("crayfish"))	zlim=c(1,8)
	if (tax==c("turtles"))	zlim=c(1,6)
	if (tax==c("frog"))	zlim=c(1,13)

	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	#text(130,-39.5,"Gains",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)
	
	}