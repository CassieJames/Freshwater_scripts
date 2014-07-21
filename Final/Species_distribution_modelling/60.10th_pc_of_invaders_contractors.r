source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

#### Set basic parameters

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
image.dir = '/home/jc246980/SDM/Richness/Images/'; setwd(image.dir)


#### Select options for future richness

taxa=c('fish','crayfish','turtles','frog'); tax=taxa[2] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

### Load R data files, determine proportions and save out
for(tax in taxa){ print(tax)
out.dir = '/home/jc246980/SDM/Invaders_contractors/'; setwd(out.dir)
load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	

	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
	load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	

	# Invaders
	outdelta=outquant_Invaders[,2:ncol(outquant_Invaders)]# make a copy
	outdelta_50=outdelta[,paste(yr, "_50", sep="")]
	outdelta=(outdelta+1)/(Richness_current[,2]+1)  # add one to both sides to avoid dividing by zero
	outdelta=cbind(Richness_current[,1],outdelta)			# Bind Segment number						# all proportions that equal 1 are actually zero 													# set all proportions that are greater than one = 1 
	colnames(outdelta)[1]<-"SegmentNo"	
	colnames(outdelta)[24]<-"PropFuture"	
	outdelta=as.data.frame(outdelta)
	outdelta$PropFuture<-ifelse(outdelta_50==0,NA,outdelta$PropFuture)# all proportions are actually NA if future is zero	
	save(outdelta,file=paste(out.dir,es, "_",tax,"_Invaders_Proportion.Rdata",sep=''))

	#Contractors
	outdelta=outquant_Contractors[,2:ncol(outquant_Contractors)]# make a copy
	outdelta=(outdelta)/(Richness_current[,2]) # divide future by current
	outdelta[which(is.nan(outdelta))]=NA
	outdelta=cbind(Richness_current[,1],outdelta)
	colnames(outdelta)[1]<-"SegmentNo"	
	colnames(outdelta)[24]<-"PropFuture"	
	outdelta=as.data.frame(outdelta)
	save(outdelta,file=paste(out.dir,es, "_",tax,"_Contractors_Proportion.Rdata",sep=''))

}
#### Determine areas with greatest immigrants and least emmigrants

for(tax in taxa){ print(tax)
###Invaders!
	    
		out.dir = '/home/jc246980/SDM/Invaders_contractors/'; setwd(out.dir)
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders_Proportion.Rdata',sep=''))
		quant = quantile(outdelta$PropFuture[which(outdelta$PropFuture>0)],0.9,na.rm=TRUE,type = 8);print(quant)
		outdelta$PropFuture2 = outdelta$PropFuture
		outdelta$PropFuture2[which(outdelta$PropFuture2>quant)] = 1
		outdelta$PropFuture2[which(outdelta$PropFuture2<quant )] = NA
		outdelta$PropFuture2[which(outdelta$PropFuture2==quant)] = 1
		save(outdelta,file=paste(out.dir,es, "_",tax,"_90thpc_of_proport_invaders.Rdata",sep=''))
				 	
		
###Contractors! 

		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors_Proportion.Rdata',sep=''))
		quant = quantile(outdelta$PropFuture[which(outdelta$PropFuture>0)],0.10,na.rm=TRUE,type = 8);print(quant)
		outdelta$PropFuture2 = outdelta$PropFuture
		outdelta$PropFuture2[which(outdelta$PropFuture2>quant)] = NA
		outdelta$PropFuture2[which(outdelta$PropFuture2>0 )] = 1
		save(outdelta,file=paste(out.dir,es, "_",tax,"_10thpc_of_proport_contractors.Rdata",sep=''))
}


############# Images

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos


taxa=c('fish','crayfish','turtles','frog'); tax=taxa[4] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085	

out.dir = '/home/jc246980/SDM/Invaders_contractors/Images/'; setwd(out.dir)

png(paste(out.dir,es,"_",tax,'_lowest_10th_I_E.png',sep=''),width=dim(base.asc)[1]*2+200, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
par(mar=c(0,3,0,3),mfrow=c(4,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters
for(tax in taxa[1:4]) {	print(tax)

	
	pos=tpos
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es, "_",tax,"_90thpc_of_proport_invaders.Rdata",sep=''))
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	tasci=make.asc(pos[,'PropFuture2'])
	zl = c(0,1)
	cols = c("blue")
	image(tasci, axes=FALSE,ann=FALSE,col=cols,zlim=zl)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
	
	pos=tpos
	load(paste('/home/jc246980/SDM/Invaders_contractors/',es, "_",tax,"_10thpc_of_proport_contractors.Rdata",sep=''))
	pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
	tascc=make.asc(pos[,'PropFuture2'])
	cols = c("blue")
	zl = c(0,1)
    image(tascc, axes=FALSE,ann=FALSE,col=cols,zlim=zl)
    plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    if (tax==c("frog")) color.legend(118,-44,140,-41,zl,cols,cex=10)
	
}
 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('Increase','Decrease'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/4,0.99,1/2))   
 dev.off()
 
 
 
 
 
 
 
 
