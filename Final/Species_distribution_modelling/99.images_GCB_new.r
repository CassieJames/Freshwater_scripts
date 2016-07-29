#### Creating images for Global Change Biology paper
# C James February 2015


##################################################################################################
###Zoom in figure 2

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile
WTshape = readShapePoly('/home/jc246980/Janet_Stein_data/WTIBRA.shp') #read in your shapefile
image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)

cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos

taxa=c('fish','crayfish','turtles','frog');tax=taxa[1] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085		

assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 135.95,146.65, -21.00, -10.55)
xlim=c(min.lon,max.lon);
ylim=c(min.lat,max.lat)


	  png(paste(es,'_',tax,'_',yr,'.zoom.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
					  par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))

					  mat = matrix(c( 1,1,1,2,
									  1,1,1,1,
									  1,1,1,1,
									  1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
					  layout(mat) #call layout as defined above

						load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
						pos=tpos
						cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
						pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
						zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
						tascCUR=make.asc(pos[,'Current'])
						image(base.asc,ann=F,axes=F,col='white', xlim=xlim,ylim=ylim)
						plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
						image(tascCUR,ann=F,axes=F,col='grey', add=TRUE, xlim=xlim,ylim=ylim,xpd=FALSE)
						tasc=make.asc(pos[,'Current'])
						image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE,xlim=xlim,ylim=ylim,xpd=FALSE)
						plot(WTshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
						color.legend(144.8,-14.0,147.4,-13.7,zlim,cols,cex=10)

						assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 111,154.5, -44, -9)

					  image(base.asc,ann=FALSE,axes=FALSE,col='grey60')
					  image(clip.image(135.95,146.65, -20.05, -10.55),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)
					  

	  dev.off()	

	
	
##################################################################################################
###Climate Figure 1

library(plotrix) 
library(SDMTools)
library(maptools)

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
cols = all.cols[21:1] # blue to red
image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
percentile=50
es=c("RCP85")
yr=2085				
data.dir="/home/jc246980/Obsolete/Stability/Output/"			
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile
pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))


		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) 
		pos=make.pos(base.asc)
	
		png(paste(image.dir,es,"_",yr,'Stability_climate.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='white')
		par(mar=c(0,0,0,0),mfrow=c(2,2),cex=1,oma=c(4,4,4,0)) #define the plot parameters
		
		
		image(base.asc,ann=F,axes=F,col='wheat1')
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		text(133.3,-19.712,"Timor Sea",cex=6)
		text(139.7,-26.085,"Lake Eyre",cex=6)
		text(127.5,-29.1,"South Western Plateau",cex=6)
		text(116.375,-24.138,"Indian",cex=6)
		text(116.375,-25.338,"Ocean",cex=6)
		text(117.496,-30.687,"South",cex=6)
		text(117.496,-31.887,"West",cex=6)
		text(117.496,-33.087,"Coast",cex=6)
		text(123.400,-21.306,"North Western",cex=6)
		text(123.400,-22.506,"Plateau",cex=6)
		text(145.406,-32.953,"Murray-Darling",cex=6)
		text(145.406,-34.153,"Basin",cex=6)
		text(141.00,-18.06,"Gulf of",cex=5.5)
		text(141.34,-19.26,"Carpentaria",cex=5.5)
		text(131.89,-36.23,"South Australian",cex=6)
		text(131.89,-37.43,"Gulf",cex=6)
		text(140.98,-42.193,"Tasmania",cex=6)
		text(151.0,-38.95,"South East",cex=6)
		text(151.0,-40.15,"Coast",cex=6)
		text(151.0,-17.588,"North East",cex=6)
		text(151.0,-18.788,"Coast",cex=6)

		pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))
		base.asc = read.asc("/home/jc246980/Obsolete/Climate/Baseline_5km/base.asc") #read in the base asc file
		pos=make.pos(base.asc)
		
		cols = all.cols[11:1] # blue to red				
		voi=c("tmp")
		outdelta=read.csv(paste(data.dir,voi,"_delta.csv", sep=''))
		deltalims=c(1.3,6)
		deltamid=round(((deltalims[1]+deltalims[2])/2),1)
		deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))	
	    tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,yr,percentile,sep='_')] #get the data
		tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) 
		pos=make.pos(base.asc)
		pos$temp_data = extract.data(cbind(pos$lon,pos$lat),tasc)
		tasc=base.asc
		tasc[cbind(pos$row,pos$col)] =pos$temp_data
		image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		legend.gradient(pnts,cols=cols,limits=deltalims, cex= 6, title='')
		text(130,-39.5,expression('Mean Daily Temperature ('^o*'C)'),cex=7)
		
		
		pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))
		base.asc = read.asc("/home/jc246980/Obsolete/Climate/Baseline_5km/base.asc") #read in the base asc file
		pos=make.pos(base.asc)
		voi=c("pre")		
		outdelta=read.csv(paste(data.dir,voi,"_delta.csv", sep=''))
		outdeltalims=outdelta[,101:103] 
		deltalim_x=round(range(outdeltalims)[1],1)
		deltalim_y=round(range(outdeltalims)[2],1)
		deltalims=c(deltalim_x,deltalim_y)
		deltalabs = c(paste('<',deltalims[1]),paste('>',deltalims[2]))
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,yr,percentile,sep='_')] #get the data
		tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) 
		pos=make.pos(base.asc)
		pos$pre_data = extract.data(cbind(pos$lon,pos$lat),tasc)
		tasc=base.asc
		tasc[cbind(pos$row,pos$col)] =pos$pre_data	
		image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		legend.gradient(pnts,cols=all.cols,limits=deltalabs,cex= 6, title='')
		text(130,-39.5,"Annual Precipitation (cm)",cex=7)
		
		all.cols=colorRampPalette(c("#A50026","#F46D43","#FEE090", "#FFFFBF","#ABD9E9","#4575B4"))(6)	
		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) 
		load(paste(data.dir,'Pos_merged_accummulated.Rdata',sep=''))
		tasc=base.asc; tasc[cbind(pos$row,pos$col)]=pos[,paste(es,yr,percentile,sep='_')]			

		tasc[which(tasc<=0.25)] = 0.01
		tasc[which(tasc<=0.5 & tasc>0.25)] = 0.02
		tasc[which(tasc>0.5 & tasc<=1)] = 0.03
		tasc[which(tasc>1 & tasc<=2)] = 0.04
		tasc[which(tasc>2 & tasc<=4)] = 0.05
		tasc[which(tasc>4)] = 0.06
		
		zlims = range(c(0,as.vector(tasc)),na.rm=TRUE)
		image(tasc, ann=FALSE,axes=FALSE,col=all.cols,zlim=zlims)
		plot(Drainageshape , lwd=8, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("<0.25",">4")
		legend.gradient(pnts,cols=all.cols, limits=labs, cex= 6, title='')
		text(130,-39.5,paste("Annual Accumulated Runoff (GL)", sep=""),cex=7)
		
		dev.off() #close out the image
		
#############################################################################################
####Turnover Figure 3

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
cols = all.cols[11:1] # blue to red		

Taxa=c('fish','crayfish','turtles','frog')
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)
pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.Turnover_plus_gains.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
	
	for (tax in Taxa) {
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]							   # extract correct 
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)

	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	if (tax==c("crayfish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("frog"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("turtles"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	#zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zzlim=c(0, 1)
	zlim=c(0.1,1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_50"])
	#image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zzlim,cols,cex=10)
	
	}
	
	dev.off()
	
	##########################################################################################
	#### Zoom in of high biodiversity hotspots - with gains in denominator
	
	
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	Ozoutline = readShapePoly('/home/jc246980/Janet_Stein_data/AustraliaOutline.shp') #read in your shapefile

	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		

	Taxa=c('fish','crayfish','turtles','frog')
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)
	pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.Turnover_zooms_gains.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
	
	#Kakadu
	#Fish
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Kakadu.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 130,134, -15, -11.5)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[1]
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	#image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
	

	# #Kimberley
	# #Fish
	# assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 121,129.4, -18.31, -13.315)
	# xlim=c(min.lon,max.lon);
	# ylim=c(min.lat,max.lat)
	# tax=Taxa[1]
	# pos=tpos
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	# if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	# zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	# zlim=round(zlim, 1)
	# pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	# image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	# tasc=make.asc(pos[,"Turnover_50"])
	# #image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	# image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	# plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	# color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	#South East Highlands
	#Cray fish
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/SEHighlandsIbra.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 146.0,151.3, -39.3, -32.3)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[2]
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("crayfish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>8)]=8
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	#image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
		
	
	#South East Queensland
	#Crayfish

	# assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 152.7,153.44, -28.24, -27.7)
	# xlim=c(min.lon,max.lon);
	# ylim=c(min.lat,max.lat)
	# tax=Taxa[2]
	# pos=tpos
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	# if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	# zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	# zlim=round(zlim, 1)
	# pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	# image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	# tasc=make.asc(pos[,"Turnover_50"])
	# #image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	# image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	# plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	# color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	#Mary River SEQ
	#Turtles
	# assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 152.5,153.18, -26.8, -25.5)
	# xlim=c(min.lon,max.lon);
	# ylim=c(min.lat,max.lat)
	# tax=Taxa[3]
	# pos=tpos
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	# if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	# zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	# zlim=round(zlim, 1)
	# pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	# image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	# tasc=make.asc(pos[,"Turnover_50"])
	# #image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	# image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	# plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	# color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	#Brigalow
	#Turtles
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Brigalow_dis.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 145.808,152.45, -29.3, -19.07)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[3]
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover absolute not proportional estimates
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("turtles"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>7)]=7
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	# #image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	# #Wet Tropics
	# #Frogs
	# assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 144.7,146.334, -19, -15.9)
	# xlim=c(min.lon,max.lon);
	# ylim=c(min.lat,max.lat)
	# tax=Taxa[4]
	# pos=tpos
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	# if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	# zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	# zlim=round(zlim, 1)
	# pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	# image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	# tasc=make.asc(pos[,"Turnover_50"])
	# #image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	# image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	# plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	# color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	#Northern NSW
	#Frogs
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/SEQNSWIBRA_dissolved.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 151.41,153.66, -32.94, -28.1)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[4]
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	# load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	# Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct 
	# Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)	
	# colnames(Turnover_50)[1]<-"SegmentNo"
	# Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	#image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, ,xlim=xlim,ylim=ylim, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	dev.off()
	
	
	
	#######################################################################################################################
	#### ##########################################################################################
	#### Zoom in of Kakadu showing invaders and contractors FISH_gains in denominator
	
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	Ozoutline = readShapePoly('/home/jc246980/Janet_Stein_data/AustraliaOutline.shp') #read in your shapefile

	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		

	Taxa=c('fish','crayfish','turtles','frog')
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)
	pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.Kakadu_zooms_gains.png',sep=''),width=dim(base.asc)[1]*1+100, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
		
	mat = matrix(c( 1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					4,4,4,4,4,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					5,5,5,5,5,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					6,6,6,6,6),nr=15,nc=5,byrow=TRUE) #create a layout matrix for images
		
	layout(mat) #call layout as defined above
	
	
	#Kakadu
	#Fish
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Kakadu.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 130,134, -15, -11.5)
	xlim=c(min.lon,max.lon)
	ylim=c(min.lat,max.lat)
	tax=Taxa[1]
	
	#Contractors
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE) # term that calculates values as proportion
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(0,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Contractor_labs=round(zlim,0)
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	Pos_invaders=pos # to create a proportional turnover
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	zlim=c(0,max(c(pos[,28]),na.rm=T))
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Invader_labs=round(zlim,0)


	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	Turnover_labs=zlim*100
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Gains",cex=12)
	color.legend(2,10,18,15,Invader_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Turnover",cex=12)
	color.legend(2,10,18,15,Turnover_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	dev.off()
	
	
	#######################################################################################################################
	#### ##########################################################################################
	#### Zoom in of SE Highlands shoring invaders and contractors Crayfish
	
		
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	Ozoutline = readShapePoly('/home/jc246980/Janet_Stein_data/AustraliaOutline.shp') #read in your shapefile

	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		

	Taxa=c('fish','crayfish','turtles','frog')
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)
	pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.SEHighlands_zooms_gains.png',sep=''),width=dim(base.asc)[1]*1+100, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
		
	mat = matrix(c( 1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					4,4,4,4,4,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					5,5,5,5,5,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					6,6,6,6,6),nr=15,nc=5,byrow=TRUE) #create a layout matrix for images
		
	layout(mat) #call layout as defined above
	
	#South East Highlands
	#Cray fish
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/SEHighlandsIbra.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 146.0,151.3, -39.3, -32.3)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[2]

		#Contractors
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE) # term that calculates values as proportion
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(0,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Contractor_labs=round(zlim,0)
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	Pos_invaders=pos # to create a proportional turnover
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	zlim=c(0,max(c(pos[,28]),na.rm=T))
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Invader_labs=round(zlim,0)


	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	Turnover_labs=zlim*100
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Gains",cex=12)
	color.legend(2,10,18,15,Invader_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Turnover",cex=12)
	color.legend(2,10,18,15,Turnover_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	dev.off()
	
#######################################################################################################################
#### Zoom in of Northern NSW shoring invaders and contractors Frogs
	
		
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	Ozoutline = readShapePoly('/home/jc246980/Janet_Stein_data/AustraliaOutline.shp') #read in your shapefile

	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		

	Taxa=c('fish','crayfish','turtles','frog')
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)
	pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.NorthNSW_zooms_gains.png',sep=''),width=dim(base.asc)[1]*1+100, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
		
	mat = matrix(c( 1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					4,4,4,4,4,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					5,5,5,5,5,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					6,6,6,6,6),nr=15,nc=5,byrow=TRUE) #create a layout matrix for images
		
	layout(mat) #call layout as defined above
	
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/SEQNSWIBRA_dissolved.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 151.41,153.66, -32.94, -28.1)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[4]

	#Contractors
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE) # term that calculates values as proportion
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(0,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Contractor_labs=round(zlim,0)
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	Pos_invaders=pos # to create a proportional turnover
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	zlim=c(0,max(c(pos[,28]),na.rm=T))
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Invader_labs=round(zlim,0)


	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	Turnover_labs=zlim*100
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Gains",cex=12)
	color.legend(2,10,18,15,Invader_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Turnover",cex=12)
	color.legend(2,10,18,15,Turnover_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	dev.off()
		
######################################################################################################################################
#### Daley rover turtles zoom in
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	Ozoutline = readShapePoly('/home/jc246980/Janet_Stein_data/AustraliaOutline.shp') #read in your shapefile

	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		

	Taxa=c('fish','crayfish','turtles','frog')
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)
	pnts=cbind(x=c(146,147.5,147.5,146),y=c(-12,-12,-16,-16))	
	
	png(paste(es,'_',yr,'.Daley_zooms_gains.png',sep=''),width=dim(base.asc)[1]*1+100, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters	
		
	mat = matrix(c( 1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					4,4,4,4,4,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					5,5,5,5,5,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					6,6,6,6,6),nr=15,nc=5,byrow=TRUE) #create a layout matrix for images
		
	layout(mat) #call layout as defined above
	
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Daley.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 130.153,133.57, -16.545, -13.2)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[3]

		#Contractors
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE) # term that calculates values as proportion
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(0,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Contractor_labs=round(zlim,0)
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	Pos_invaders=pos # to create a proportional turnover
	#pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	zlim=c(0,max(c(pos[,28]),na.rm=T))
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	Invader_labs=round(zlim,0)


	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current	# extract correct 
	Turnover_50=cbind(Richness_current[,1],Turnover_50)	
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>30)]=30
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	zlim=round(zlim, 1)
	Turnover_labs=zlim
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	tasc=make.asc(pos[,"Turnover_50"])
	plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,xlim=xlim,ylim=ylim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Gains",cex=12)
	color.legend(2,10,18,15,Invader_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Turnover",cex=12)
	color.legend(2,10,18,15,Turnover_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	dev.off()		
		
##################################################################################################
#### Appendix figures
	
####################################################################################################################################################################
##### Contractors image

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)

es='RCP85'
yr=2085
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
taxa=c('fish','crayfish','turtles','frog')
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

PERC=c('10','50','90'); percentile=PERC[2]		
	png(paste(es,'_contractors_',yr,'.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	### Current richness
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_Richness_current.mat.Rdata",sep='')) # Current
	Richness_current[,2]=Richness_current[,2]/max(c(Richness_current[,2]),na.rm=T) # Make richness a proportion of maximum
	pos=tpos	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(pos$Current[which(pos$Current>0)],na.rm=T),max(c(pos$Current),na.rm=T))
	tascCur=make.asc(pos[,'Current'])
	tascCur[which(tascCur>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)

	### Future contractors
	cols=colorRampPalette(c("#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(min(doi[which(doi>0)],na.rm=T),max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)

	###Contractors as a proportion
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_Richness_current.mat.Rdata",sep='')) # Current
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep='')) # Future
	outdelta=outquant_Contractors[,2:ncol(outquant_Contractors)]# make a copy
	outdelta=(outdelta)/(Richness_current[,2]) # divide future by current
	outdelta[which(is.nan(outdelta))]=NA
	outdelta=cbind(Richness_current[,1],outdelta)
	colnames(outdelta)[1]<-"SegmentNo"	
	pos=tpos
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	tasc[which(tasc>=1)] = 1
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	zlim=c(0,1)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=c("0","1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)
}
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('1990','2085 Contractors','Proportion'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
 
 dev.off()

########################################################################################################################################################
#### Invaders image
	
	
	PERC=c('10','50','90'); percentile=PERC[2]		
	
	png(paste(es,'_invaders_',yr,'.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	### Current richness
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_Richness_current.mat.Rdata",sep='')) # Current
	Richness_current[,2]=Richness_current[,2]/max(c(Richness_current[,2]),na.rm=T) # Make richness a proportion of maximum
	pos=tpos	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(pos$Current[which(pos$Current>0)],na.rm=T),max(c(pos$Current),na.rm=T))
	tascCur=make.asc(pos[,'Current'])
	tascCur[which(tascCur>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)

	### Future contractors
	cols=colorRampPalette(c("#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)
	pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(min(doi[which(doi>0)],na.rm=T),max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)

	###Contractors as a proportion
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_Richness_current.mat.Rdata",sep='')) # Current
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep='')) # Future
	outdelta=outquant_Invaders[,2:ncol(outquant_Invaders)]# make a copy
	outdelta=(outdelta)/(Richness_current[,2]) # divide future by current
	outdelta[which(is.nan(outdelta))]=NA
	outdelta=cbind(Richness_current[,1],outdelta)
	colnames(outdelta)[1]<-"SegmentNo"	
	pos=tpos
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	tasc[which(tasc>=1)] = 1
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCur,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	zlim=c(0,1)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=c("0","1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)
}
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('1990','2085 Invaders','Proportion'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
 
 dev.off()
