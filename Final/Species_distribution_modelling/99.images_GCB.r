#### Creating images for Global Change Biology paper
# C James December 2013

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos

image.dir = '/home/jc246980/SDM/Richness/Images/'; setwd(image.dir)
cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)

####################################################################################################################################################################
#### Richness image

taxa=c('fish','crayfish','turtles','frog') # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

	png(paste(es,'_richness_Figure1.1_',yr,'.Oct15.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)
		
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	Richness_current[,2]=Richness_current[,2]/max(c(Richness_current[,2]),na.rm=T) # Make richness a proportion of maximum
	pos=tpos
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(pos$Current[which(pos$Current>0)],na.rm=T),max(c(pos$Current),na.rm=T))
	tasc=make.asc(pos[,'Current'])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'.Richness_quants.Rdata',sep='')) # Future
	pos=tpos
	pos=merge(pos,outquant_Richness, by='SegmentNo',all.x=TRUE)
	pos[,paste(yr,'_',percentile,sep='')]=pos[,paste(yr,'_',percentile,sep='')]/max(pos[,paste(yr,'_',percentile,sep='')], na.rm=TRUE)
	doi=pos[,paste(yr,'_',percentile,sep='')]
	zlim=c(min(doi[which(doi>0)],na.rm=T),max(c(pos[,paste(yr,'_',percentile,sep='')]),na.rm=TRUE))	
	tasc=make.asc(pos[,paste(yr,'_',percentile,sep='')])
	tasc[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',percentile,sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    labs=c("<=0.01", "1")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)
	
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",'RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'.Richness_quants.Rdata',sep='')) # Future
	deltalabs=c('0','>=2')
	outdelta=outquant_Richness[,2:ncol(outquant_Richness)]# make a copy	
	outdelta=outdelta/Richness_current[,2]
	outdelta[which(is.nan(outdelta))]=NA
	outdelta[which(outdelta>2)]=2
	zlim=c(0,2)
	outdelta=cbind(outquant_Richness[,1],outdelta)
	colnames(outdelta)[1]<-"SegmentNo"
	pos=tpos
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',50,sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	if (tax==c("frog")) color.legend(118,-44,140,-41,deltalabs,cols,cex=10)
	
	
	
	

	}
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('1990','2085','Proportion'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
 
 dev.off()
 
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
image.dir = '/home/jc246980/SDM/Invaders_contractors/Images/'; setwd(image.dir)
taxa=c('fish','crayfish','turtles','frog')
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


	png(paste(es,'_contractors_Figure2.1_',yr,'.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	### Current richness
	load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep='')) # Current
	Richness_current[,2]=Richness_current[,2]/max(c(Richness_current[,2]),na.rm=T) # Make richness a proportion of maximum
	pos=tpos	
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(pos$Current[which(pos$Current>0)],na.rm=T),max(c(pos$Current),na.rm=T))
	tascCur=make.asc(pos[,'Current'])
	tascCur[which(tasc>0)] = NA
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
	load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep='')) # Current
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
	
	png(paste(es,'_Invaders_Figure3.1_',yr,'.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	zlim=c(min(c(pos$Current),na.rm=T)+1,max(c(pos$Current),na.rm=T))
	tascCUR=make.asc(pos[,'Current'])
	tascCUR[which(tasc>0)] = NA
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,'Current'])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	cols=colorRampPalette(c("#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(6)
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	pos=tpos
	pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
	zlim=c(1,max(c(pos[,28]),na.rm=T))
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_50',sep='')])
	if (tax=="fish") tasc[which(tasc>10)] = 10; 
	if (tax=="fish") zlim=c(1,10)			 	
	tasc[which(tasc<1)] = NA
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=zlim
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	
	outdelta=outquant_Invaders[,2:ncol(outquant_Invaders)]	# make a copy
	outdelta_50 = outdelta[,paste(yr,'_50',sep='')]			# extract raw future data
	outdelta=(outdelta+1)/(Richness_current[,2]+1)  		# create proportions but add one to both sides to avoid dividing by zero														
	outdelta=cbind(Richness_current[,1],outdelta)			# Bind Segment number
	colnames(outdelta)[1]<-"SegmentNo"	
	colnames(outdelta)[24]<-"PropFuture"					
	outdelta=as.data.frame(outdelta)
	outdelta$PropFuture<-ifelse(outdelta_50==0,NA,outdelta$PropFuture)# all proportions are actually NA if future is zero	
	pos=tpos
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)	    # Merge with position file to create asc
	
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,"PropFuture"])
	tasc[which(tasc>=2)] = 2								# Change any proportions that are > 2 to 2	
	zlim=c(0,2)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)   # Add image
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	labs=c("0",">=2")
	if (tax==c("frog")) color.legend(118,-44,140,-41,labs,cols,cex=10)
	}
	
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('1990','2085 Invaders','Proportion'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/6,0.99,1/3))
 
 dev.off()
 
 
