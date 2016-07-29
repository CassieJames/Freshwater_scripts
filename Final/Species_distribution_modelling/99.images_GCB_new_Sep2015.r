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

taxa=c('fish','crayfish','turtles','frog');tax=taxa[4] # select species
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


	png(paste(es,'_',tax,'_',yr,'.Sep2015.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters

	mat = matrix(c( 1,1,1,1,1,2,2,
				    1,1,1,1,1,2,2,
					1,1,1,1,1,3,3,
					1,1,1,1,1,3,3,
					1,1,1,1,1,4,4,
					1,1,1,1,1,4,4),nr=6,nc=7,byrow=TRUE) #create a layout matrix for images
    layout(mat) #call layout as defined above 
	
	# Turnover plot
	
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]							  								# extract column of interest 
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_50=cbind(Richness_current[,1],Turnover_50)
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)

	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	if (tax==c("crayfish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("frog"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("turtles"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10

	zzlim=c(0, 1)
	zlim=c(0.1,1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_50"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zzlim,cols,cex=10)
	text(130,-39.5,"Turnover",cex=20)
	
	
	#Current
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
	tascCUR=make.asc(pos[,'Current'])
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    text(130,-39.5,"Current",cex=12)
	color.legend(118,-42,140,-41,zlim,cols,cex=6)
	
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
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	text(130,-39.5,"Gains",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)
	
	# Contractors

	
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)	
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(1,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))		
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	text(130,-39.5,"Losses",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)

	dev.off()
	
	
	########################################################################################
	### Code to calculate turnover for each taxa, drainage and overALL
	
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(maptools)
	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load DRAINAGE DIVISIONS
	pos$drainage=extract.data(cbind(pos$lon,pos$lat),RiverBasin.asc )
	Drainages = unique(na.omit(pos$drainage)) # create drainage vector

	taxa=c('fish','crayfish','turtles','frog');tax=taxa[4] # select species
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)

	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]							  								# extract column of interest 
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_50=cbind(Richness_current[,1],Turnover_50)
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)

	posdat=merge(Turnover_50,pos,by='SegmentNo')

	
	out.dir= "/home/jc246980/Obsolete/Final report/Summary_data/"	

	table_delta = matrix(NA,nrow=length(Drainages),ncol=4); #define the output matrix
	colnames(table_delta) = c("segno",'quant_10', 'quant_50', 'quant_90') 
	rownames(table_delta)=Drainages
	
		
	for (rb in Drainages) { cat(rb,'\n') #cycle through each basin
					
		out_rb = posdat[which(posdat$drainage==rb),] #get the data only for the rb of interest
		out_rb=out_rb[,c(1,2,7)]
		deduped.data <- unique( out_rb )
		outquant = t(apply(as.data.frame(deduped.data[,"Turnover_50"]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))#get the percentiles					
		table_delta[rownames(table_delta)==rb,2:4]=outquant[,]
		table_delta[rownames(table_delta)==rb,'segno']=nrow(deduped.data)
		
	}; cat('\n')
			
		

		write.csv(table_delta,paste(out.dir,"Frogs_turnover_summary.csv",sep=''),row.names=T)	 
		

		##########################################################################################
		### zoom figures
		
		
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


PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085		

assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 138,146.65, -21.00, -10.55)
xlim=c(min.lon,max.lon);
ylim=c(min.lat,max.lat)


	  png(paste(es,'_Figure 1_',yr,'.zoom.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
	  par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0), bg = 'white')

		mat = matrix(c( 1,1,1,1,1,2,2,2,2,
					    1,1,1,1,1,2,2,2,2,
					    1,1,1,1,1,2,2,2,2,
					    1,1,1,1,1,1,1,1,2,
					    1,1,1,1,1,1,1,1,3,
						1,1,1,1,1,1,1,1,3,
						1,1,1,1,1,1,1,1,3),nr=7,nc=9,byrow=TRUE) #create a layout matrix for images
		layout(mat) #call layout as defined above

		

		assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 138.95,148.00, -21.00, -10.55)
		xlim=c(min.lon,max.lon);
		ylim=c(min.lat,max.lat)			
		
		
		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
		pos=make.pos(base.asc)		
		tax=c('fish')
		load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
		pos=tpos
		cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
		pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
		zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
		tascCUR=make.asc(pos[,'Current'])
		image(base.asc,ann=F,axes=F,col='white', xlim=xlim,ylim=ylim)
		image(tascCUR,ann=F,axes=F,col='grey', add=TRUE, xlim=xlim,ylim=ylim,xpd=FALSE)
		tasc=make.asc(pos[,'Current'])
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE,xlim=xlim,ylim=ylim,xpd=FALSE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		text(138.4,-11.35,"(a)",cex=14)
		color.legend(146.5,-17.5,149,-17.2,zlim,cols,cex=10)
 
		image(base.asc,ann=F,axes=F,col='wheat1')
		box(lty = "solid")
		image(clip.image(138,146.65, -20.05, -10.55),ann=FALSE,axes=FALSE, col="grey80",add=TRUE)
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
		text(115,-10.56,"(b)",cex=14)
					  
	  dev.off()	

	
	##### Create plots


	png(paste(es,'_',tax,'_',yr,'.Sep2015.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters

	mat = matrix(c( 1,1,1,1,1,2,2,
				    1,1,1,1,1,2,2,
					1,1,1,1,1,3,3,
					1,1,1,1,1,3,3,
					1,1,1,1,1,4,4,
					1,1,1,1,1,4,4),nr=6,nc=7,byrow=TRUE) #create a layout matrix for images
    layout(mat) #call layout as defined above 
	
	# Turnover plot
	
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)
	cols = all.cols[11:1] # blue to red		
	pos=tpos
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants_gains.Rdata',sep='')) # Load turnover
	yois=c(2015, 2025, 2035, 2045, 2055, 2065, 2075, 2085)
	tt = expand.grid(c("10%", "50%", "90%"),yois=yois);tt = paste(tt[,2],tt[,1],sep='_'); colnames(outquant_Turnover) = tt #add the column names
	Turnover_50 = outquant_Turnover[,paste(yr,'_50%',sep='')]							  								# extract column of interest 
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) 				# Load current file and grab segment numbers off it
	Turnover_50=cbind(Richness_current[,1],Turnover_50)
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)

	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>20)]=20
	if (tax==c("crayfish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("frog"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10
	if (tax==c("turtles"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>10)]=10

	zzlim=c(0, 1)
	zlim=c(0.1,1)
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_50"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zzlim,cols,cex=10)
	text(130,-39.5,"Turnover",cex=20)
	
	
	#Current
	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(20)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
	tascCUR=make.asc(pos[,'Current'])
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    text(130,-39.5,"Current",cex=12)
	color.legend(118,-42,140,-41,zlim,cols,cex=6)
	
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
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])	 	
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	text(130,-39.5,"Gains",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)
	
	# Contractors

	
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)	
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(1,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))		
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=ceiling(zlim)
	text(130,-39.5,"Loses",cex=12)
	color.legend(118,-42,140,-41,labs,cols,cex=6)

	dev.off()
