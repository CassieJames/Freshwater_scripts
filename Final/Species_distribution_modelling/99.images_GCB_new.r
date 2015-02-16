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

	png(paste(es,'_richness_Figure1.1_',yr,'.Nov27_bioregionclip.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)
		
	if (tax==c("fish")) es='RCP85Bioclip'	
	
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

PERC=c('10','50','90'); percentile=PERC[2]		
	png(paste(es,'_contractors_Figure2.1_',yr,'Nov28.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
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
	
	png(paste(es,'_Invaders_Figure3.1_',yr,'Nov28.png',sep=''),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(4,3),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_Richness_current.mat.Rdata",sep='')) 
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
 #New Images 1st December
 ####################################################################################################################################################################
#### Richness image

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos

image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)


taxa=c('fish','crayfish','turtles','frog');tax=taxa[4] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

	png(paste(es,'_',tax,'_',yr,'.withagree5.png',sep=''),width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+150, units='px', pointsize=20, bg='WHITE')	
    par(mar=c(0,3,0,3),mfrow=c(2,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters

	#Current
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
    color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	#Future as a proportion of current
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",'RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'.Richness_quants.Rdata',sep='')) # Future
	deltalabs=c('0','>=2')
	outdelta=outquant_Richness[,2:ncol(outquant_Richness)]# make a copy	
	outdelta=outdelta/Richness_current[,2]
	outdelta[which(outdelta>2)]=2
	zlim=c(0,2)
	outdelta=cbind(outquant_Richness[,1],outdelta)
	colnames(outdelta)[1]<-"SegmentNo"
	pos=tpos
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	tasc=make.asc(pos[,paste(yr,'_',50,sep='')])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,deltalabs,cols,cex=10)
	
	#turnover plot
	load(paste('/home/jc246980/SDM/TurnOver/',es,"_",tax,'_Turnover_quants.Rdata',sep='')) # Load turnover
	Turnover_50 = outquant_Turnover[,paste(yr,'_50',sep='')]							   # extract correct year and percentile
	Turnover_50=cbind(outquant_Turnover[,1],Turnover_50)								   #bind with position data
	colnames(Turnover_50)[1]<-"SegmentNo"
	Turnover_50=as.data.frame(Turnover_50)
	zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)],na.rm=T),max(c(Turnover_50$Turnover_50),na.rm=T))
	if (tax==c("fish")) zlim=c(min(Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>0)]),5)
	if (tax==c("fish"))  Turnover_50$Turnover_50[which(Turnover_50$Turnover_50>5)]=5
	zlim=round(zlim,2)
	
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Load current
	pos=tpos
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE) # create position file with current data so that areas with 0 richness now and 1 species changes are NA as these do not contain records
	pos=merge(pos,Turnover_50, by='SegmentNo',all.x=TRUE)
	pos$Turnover_50[which(pos$Turnover_50==1 & pos$Current==0)]=NA
	image(base.asc,ann=F,axes=F,col='white')
	tasc=make.asc(pos[,"Turnover_50"])
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)
	
	# number of agree and disagree models
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/',es,"_",yr,".Richness_agree.Rdata",sep='')) # Current
	pos=tpos
	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#74ADD1","#4575B4","#313695"))(20)
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
	#pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	#tascCUR=make.asc(pos[,'Current'])
	
	tdata=Model_agree[,2]
	tdata[which(tdata<9)] = NA
	ttdata=cbind(Richness_current[,1], tdata)
	colnames(ttdata)[1]<- "SegmentNo"
	pos=merge(pos,ttdata, by='SegmentNo',all.x=TRUE)
	image(base.asc,ann=F,axes=F,col='white')
	image(tascCUR,ann=F,axes=F,col='grey', add=TRUE)	
	tasc=make.asc(pos[,"tdata"])
	zlim=c(-18, 18)
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	
	tdata=Model_agree[,3]
	tdata[which(tdata>-9)] = NA
	ttdata=cbind(Richness_current[,1], tdata)
	colnames(ttdata)[1]<-"SegmentNo"
	pos=merge(pos,ttdata, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,"tdata.y"])
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	color.legend(118,-42,140,-41,zlim,cols,cex=10)

	
	dev.off()
	
	
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
###Climate figure 1

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
		#outdelta=read.csv(paste(data.dir,voi,"_delta.csv", sep=''))
		deltalims=c(1.3,6)
		deltamid=round(((deltalims[1]+deltalims[2])/2),1)
		deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
	    tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,yr,percentile,sep='_')] #get the data
		tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
		image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		legend.gradient(pnts,cols=cols,limits=deltalims, cex= 6, title='')
		text(130,-39.5,expression(paste("Mean Daily Temperature"*~degree*C, sep="")),cex=7)
		
		voi=c("pre")		
		outdelta=read.csv(paste(data.dir,voi,"_delta.csv", sep=''))
		outdeltalims=outdelta[,101:103] 
		deltalim_x=round(range(outdeltalims)[1],1)
		deltalim_y=round(range(outdeltalims)[2],1)
		deltalims=c(deltalim_x,deltalim_y)
		deltamid=round(((deltalims[1]+deltalims[2])/2),1)
		deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,yr,percentile,sep='_')] #get the data
		tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
		image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		legend.gradient(pnts,cols=all.cols,limits=deltalims, cex= 6, title='')
		text(130,-39.5,paste("Annual Precipitation (cm)", sep=""),cex=7)
		
		base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
		pos=make.pos(base.asc)
		pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	
		load(paste(data.dir,"Accumulated_runoff_delta.Rdata",sep=''))
		pos=merge(pos,outdelta,by='SegmentNo',all.x=TRUE)
		outdeltalims=pos[,paste(es,yr,percentile,sep='_')]
		tasc=base.asc; tasc[cbind(pos$row,pos$col)]=outdeltalims[,paste(es,yr,percentile,sep='_')]			
		tasc[which(tasc<0.25)] = 0.15
		tasc[which(tasc<0.5 & tasc>= 0.25)] = 0.25
		tasc[which(tasc>=0.5 & tasc<1)] = 0.35
		tasc[which(tasc>=1 & tasc<2)] = 0.35
		tasc[which(tasc>=2 & tasc<10)] = 0.45
		tasc[which(tasc>=4)] = 0.55
		zlims = range(c(0,as.vector(tasc)),na.rm=TRUE)
		image(tasc, ann=FALSE,axes=FALSE,col=all.cols,zlim=zlims)
		plot(Drainageshape , lwd=8, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("<0.25","0.5","1","2",">4")
		legend.gradient(pnts,cols=all.cols,limits=zlims, cex= 6, title='')
		
		dev.off() #close out the image
	