#### Creating images for current  suitable climate for crab
# C James March 2015

	
	source('/home/jc148322/scripts/libraries/cool_functions.r') # make pos function here!
	library(SDMTools);library(plotrix); library(maptools)
	
	tax=c('crab')
	image.dir=paste('/home/jc246980/SDM/Realized/Images/',tax,sep='')
	wd=paste('/home/jc246980/SDM/models_crab/Austrothelphusa_transversa/output/potential/',sep='')
	crab_cur=read.csv(paste(wd,"current_1990.csv",sep=''))
	save(crab_cur,file=paste("/home/jc246980/SDM/Realized/crab/Crayfish_reach.Rdata",sep=''))
	crab_cur=crab_cur[,2:3]
	colnames(crab_cur)[1]="SegmentNo"
	colnames(crab_cur)[2]="Current"
	
	thresdir = paste("/home/jc246980/SDM/models_crab/Austrothelphusa_transversa/output/",sep='')
	threshold = read.csv(paste(thresdir,'maxentResults.csv',sep=''))#; threshold = threshold[which(threshold$Species==spp),]
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value	
	crab_cur[which(crab_cur[,2]<=threshold),2]=0 #clip anything below threshold to 0	
	
	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(50)
    cols=cols[50:1]	
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Level2Catchments/NCBLevel2Drainage.shp') #read in your shapefile
	species.name="Austrothelphusa transversa"
	
	png(paste(image.dir,"/",species.name,'_current_March2015.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
	mat = matrix(c( 3,2,2,2,
					1,1,1,1,
					1,1,1,1,
					1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
    layout(mat) #call layout as defined above

	pos=tpos
	pos=merge(pos,crab_cur, by='SegmentNo',all.x=TRUE)
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
	
##Alternative plot showing recorded locations

species_data=read.csv("/home/jc246980/Species_data/ALA_downloads/Data/Crab/Austrothelphusa_transversa.csv")
load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")
       

	
		
		png(paste(image.dir,"/Crab_locations.png", sep=""),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
                              par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))

                              mat = matrix(c( 2,2,2,2,
											  1,1,1,1,
											  1,1,1,1,
											  1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
                              
							  
							  
							  layout(mat) #call layout as defined above

							  
							 image(base.asc, ann=FALSE,axes=FALSE,col='white')
                              image(base.asc, ann=FALSE,axes=FALSE,col='darkgrey',add=TRUE)

                              plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='grey93', lwd=1.5)
                              plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')                              
							  points(species_data[,'LONGDEC'],species_data[,'LATDEC'],pch=16,cex=5, col='black')
							  
				
							  plot(1:20,axes=FALSE,ann=FALSE,type='n')
                              text(7,15,"Austrothelphusa transversa",cex=30)

              dev.off()