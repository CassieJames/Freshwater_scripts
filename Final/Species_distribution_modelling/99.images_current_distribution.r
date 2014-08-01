#### Creating images for current clipped distributions
# C James February 2013
	args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #retrieve and evaluate the arguments
	
	source('/home/jc148322/scripts/libraries/cool_functions.r') # make pos function here!
	library(SDMTools);library(plotrix); library(maptools)
	
	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(50)
    cols=cols[50:1]	
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Level2Catchments/NCBLevel2Drainage.shp') #read in your shapefile
	species.name=sub(".cur.real.mat.Rdata","", spp)	
	load(paste(wd,spp, sep='')) # Current		

	
	png(paste(image.dir,species.name,'_current_1August.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
	mat = matrix(c( 3,2,2,2,
					1,1,1,1,
					1,1,1,1,
					1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
    layout(mat) #call layout as defined above

	pos=tpos
	pos=merge(pos,distdata, by='SegmentNo',all.x=TRUE)
	zlimits_current=pos$Current[which(pos$Current>0)]
	zlim=c(min(zlimits_current,na.rm=TRUE),max(zlimits_current,na.rm=T))
	image(base.asc,ann=F,axes=F,col='grey')
	
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	
	species.name=sub("_"," ", species.name)
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(7,15,species.name,cex=30)
	dev.off()
	
