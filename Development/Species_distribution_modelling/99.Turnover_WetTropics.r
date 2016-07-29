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
	
	png(paste(es,'_',yr,'.wettropics_zooms.png',sep=''),width=dim(base.asc)[1]*1+100, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='WHITE')	
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
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/WTIBRA.shp') #read in your shapefile
	assign.list(min.lon,max.lon,min.lat,max.lat) %=% c( 144.863,146.71, -19.45, -15.55)
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
	tax=Taxa[1]

	pos=tpos
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
	Contractor_labs_fish=round(zlim,0)
	
	#Cray fish
	
	# tax=Taxa[2]
	# pos=tpos
	# load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	# pos=tpos
	# cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	# cols=cols[11:1]
	# pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
	# #pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE) # term that calculates values as proportion
	# doi=pos[,paste(yr,'_',percentile,sep='')]
	# zlim=c(0,max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	# image(base.asc,ann=F,axes=F,col='white',xlim=xlim,ylim=ylim)
	# tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	# plot(Ozoutline, lwd=10, ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim, add=TRUE)
	# image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE, xlim=xlim,ylim=ylim)
	# plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE, xlim=xlim,ylim=ylim)
	# Contractor_labs=round(zlim,0)
	#Turtles

	tax=Taxa[3]
	pos=tpos
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
	Contractor_labs_turtles=round(zlim,0)

	#Frogs

	tax=Taxa[4]
	pos=tpos
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
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Fish losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs_fish,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Turtle losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs_turtles,rect.col=cols,align="rb",gradient="x", cex=6)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(10,18,"Frog losses",cex=12)
	color.legend(2,10,18,15,Contractor_labs,rect.col=cols,align="rb",gradient="x", cex=6)
	
	dev.off()
	

	
	