
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

taxa=c('fish','crayfish','turtles','frog');tax=taxa[4] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

#Image


png(paste(es,'_',tax,'_',yr,'.Jan2016.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	
	# Predicted richness
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'.Richness_quants.Rdata',sep='')) # Future
	pos=tpos
	pos=merge(pos,outquant_Richness, by='SegmentNo',all.x=TRUE)
	doi=pos[,paste(yr,'_10',sep='')]
	zlim=c(min(doi[which(doi>0)],na.rm=T),max(c(pos[,paste(yr,'_90',sep='')]),na.rm=TRUE))	
	tasc=make.asc(pos[,paste(yr,'_10',sep='')])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_10',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	tasc[which(tasc>0)] = NA
	tascCur=tasc
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)

	tasc=make.asc(pos[,paste(yr,'_90',sep='')])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_90',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	pnts=cbind(x=c(148.50,151.00,151.00,148.50),y=c(-18.5,-18.5,-10.5,-10.5))
	zlim=ceiling(zlim)
	legend.gradient(pnts,col=cols,zlim,title="",cex=10)
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
	tascCUR=make.asc(pos[,'Current'])
	
	# Invaders
	
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
	tasc=make.asc(pos[,paste(yr,'_10',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)


	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)

	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_90',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	pnts=cbind(x=c(148.50,151.00,151.00,148.50),y=c(-18.5,-18.5,-10.5,-10.5))
	zlim=ceiling(zlim)
	legend.gradient(pnts,col=cols,zlim,title="",cex=10)
	
	
	# Contractors

	
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)	
	zlim=c(1,max(c(pos[,paste(yr,'_90',sep='')]),na.rm=TRUE))		
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_10',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)

	
	zlim=c(1,max(c(pos[,paste(yr,'_90',sep='')]),na.rm=TRUE))		
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)

	
	zlim=c(1,max(c(pos[,paste(yr,'_90',sep='')]),na.rm=TRUE))		
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_90',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	pnts=cbind(x=c(148.50,151.00,151.00,148.50),y=c(-18.5,-18.5,-10.5,-10.5))
	zlim=ceiling(zlim)
	legend.gradient(pnts,col=cols,zlim,title="",cex=10)

	# Turnover

	
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]							  								# extract column of interest 
	Turnover_10 = outquant_Turnover[,paste(yr,'_10%',sep='')]	
	Turnover_90 = outquant_Turnover[,paste(yr,'_90%',sep='')]	
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_50=cbind(Richness_current[,1],Turnover_50)
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)

	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_10=cbind(Richness_current[,1],Turnover_10)
	colnames(Turnover_10)[1]<-"SegmentNo"
	Turnover_10=as.data.frame(Turnover_10)
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_90=cbind(Richness_current[,1],Turnover_90)
	colnames(Turnover_90)[1]<-"SegmentNo"
	Turnover_90=as.data.frame(Turnover_90)
	
	zzlim=c(0, 1)
	zlim=c(0.1,1)
	pos=merge(pos,Turnover_10, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_10"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)


	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_50"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	
	
	pos=merge(pos,Turnover_90, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_90"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	pnts=cbind(x=c(148.50,151.00,151.00,148.50),y=c(-18.5,-18.5,-10.5,-10.5))
	zlim=c(0,1)
	legend.gradient(pnts,col=cols,zlim,title="",cex=10)


 mtext(c("Turnover","Losses", "Gains", "Richness 2085"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('10th','50th','90th'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
	
	dev.off()
