#### Creating images for species richness
# C James November 2013

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

#### Set basic parameters

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos

image.dir = '/home/jc246980/SDM/Richness/Images/'; setwd(image.dir)
cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)

#### Select options for future richness

taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)


#### Images for current richness

png(paste('Rich_current_newfile',tax,'.png',sep=''),width=dim(base.asc)[1]*2+50,height=dim(base.asc)[2]*2+200,units='px', pointsize=30, bg='white')
		par(mfrow=c(2,2),mar=c(2,2,2,2))
	
    load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/Richness_current.mat.Rdata',sep=''))
	#load(paste('/home/jc246980/SDM/Richness/Obsolete/',tax,'Richness_current.mat.Rdata',sep=''))
	zlim=c(0,round(max(Richness_current[,'Current'])))	
	pos=tpos
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=4)
dev.off()




#### Images for future richness


png(paste('Rich_future_',tax,"_",es, "_",yr,'.png',sep=''),width=dim(base.asc)[1]*2+50,height=dim(base.asc)[2]*2+200,units='px', pointsize=30, bg='white')
	par(mfrow=c(2,2),mar=c(2,2,2,2))
	
	pos=tpos
	pos=merge(pos,outquant_Richness, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',percentile,sep='')])
	zlim = range(c(0,as.vector(tasc)),na.rm=TRUE)	
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    color.legend(118,-42,140,-41,zlim,cols,cex=4)
	
dev.off()

#### Fish richness current and future images for talk


png(paste(es,'_richness_fish_',yr,'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*2, units='px', pointsize=20, bg='lightgrey')
	par(mar=c(0,0,0,0),cex=1,oma=c(3,3,3,0)) #define the plot parameters
	mat = matrix(c( 1,1,1,1,2,2,2,2,
						 1,1,1,1,2,2,2,2,
						 1,1,1,1,2,2,2,2,
						 1,1,1,1,2,2,2,2),nr=4,nc=8,byrow=TRUE) #create a layout matrix for images
		
	layout(mat) #call layout as defined above

    load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/Richness_current.mat.Rdata',sep=''))
	zlim=c(1,61)

	pos=tpos
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,'Current'])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    color.legend(118,-42,140,-41,zlim,cols,cex=5)
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/fish/',es,'.Richness_quants.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Richness, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',percentile,sep='')])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',percentile,sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    color.legend(118,-42,140,-41,zlim,cols,cex=5)
	
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    #color.legend(118,-42,140,-41,zlim,cols,cex=5)

dev.off()
	
#### fish images proportional change between current and future


	png(paste(es,'_richness_Proportion_allV1_',yr,'.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters
	deltalabs=c('0','>=2')
              
for (tax in taxa) {	print(tax)
		
		load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
		load(paste('/home/jc246980/SDM/Richness/',es,"_",tax,'_Richness_quants.Rdata',sep=''))
		outdelta=outquant_Richness[,2:ncol(outquant_Richness)]# make a copy

		outdelta=outdelta/Richness_current[,2]
		outdelta[which(is.nan(outdelta))]=NA
		outdelta[which(outdelta>2)]=2
		zlim=c(0,2)
		outdelta=cbind(outquant_Richness[,1],outdelta)
		colnames(outdelta)[1]<-"SegmentNo"
		pos=tpos
		pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
			
		tasc=make.asc(pos[,paste(yr,'_',10,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		
		tasc=make.asc(pos[,paste(yr,'_',50,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		
		tasc=make.asc(pos[,paste(yr,'_',90,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		if (tax==c("frog")) color.legend(118,-44,140,-41,deltalabs,cols,cex=10)
}		
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('10th','50th','90th'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
 	
dev.off()	
	