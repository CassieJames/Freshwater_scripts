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
cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)

# Image with invaders and contractors

for (tax in taxa) {print(tax)
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	

		
		png(paste(es,'_Invaders_',yr,'_',tax,'_10.png',sep=''),width=dim(base.asc)[1]*2+200,height=dim(base.asc)[2]*4+150,units='px', pointsize=30, bg='lightgrey')
		 par(mar=c(0,3,0,3),mfrow=c(4,2),cex=1,oma=c(10,10,10,0)) 
		
		zlim=c(0,10)	
		pos=tpos
		
		pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)	
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		tasc[which(tasc>10)] = 10
		image(base.asc,ann=F,axes=F,col='white')				 	# background for continent			# Grey is all areas that were modelled
		tasc[which(tasc<1)] = NA
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)     # Add all values that are >0 (should leave areas of no change as grey i.e. zeros)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("0",">=10")
		if (tax==frogs) color.legend(118,-42,140,-41,labs,cols,cex=4)
		
		pos=tpos
		cols=cols[11:1]
		pos=merge(pos,outquant_Contractors, by='SegmentNo',all.x=TRUE)
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		tasc[which(tasc>10)] = 10
		image(base.asc,ann=F,axes=F,col='white')
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("0",">=10")
		if (tax==c("frog")) color.legend(118,-42,140,-41,labs,cols,cex=4)
		
		
			
}
dev.off()


png(paste(es,'_Invaders_Contractors_allspecies_labelled.png',sep=''),width=dim(base.asc)[1]*2+200,height=dim(base.asc)[2]*4+150,units='px', pointsize=30, bg='white')
par(mar=c(0,3,0,3),mfrow=c(4,2),cex=1,oma=c(10,10,10,0)) 
for (tax in taxa) {print(tax)
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
	
		###Invaders
		cols=colorRampPalette(c("#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(6)
		pos=tpos
		outdelta=outquant_Invaders[,2:ncol(outquant_Invaders)]	# make a copy
		outdelta=(outdelta+1)/(Richness_current[,2]+1)  		# add one to both sides to avoid divining by zero
		outdelta[which(outdelta==1)]=NA 						# all proportions that equal 1 are actually zero (no change) 												
		outdelta=cbind(Richness_current[,1],outdelta)			# Bind segment number
		colnames(outdelta)[1]<-"SegmentNo"	
		pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)	    # Merge with position file to create asc
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		tasc[which(tasc>=2)] = 2								# Change any proportions that are > 2 to 2
		image(base.asc,ann=F,axes=F,col='white')				# background for continent			
		zlim=c(0,2)
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)   # Add image
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("0",">=2")
		if (tax==c("frog")) color.legend(118,-42,140,-41,labs,cols,cex=8)
		
		###Contractors
		pos=tpos
		cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(6)
		cols=cols[6:1]
		outdelta=outquant_Contractors[,2:ncol(outquant_Contractors)]# make a copy
		outdelta=(outdelta)/(Richness_current[,2]) # divide future by current
	    outdelta[which(is.nan(outdelta))]=NA
		outdelta=cbind(Richness_current[,1],outdelta)
		colnames(outdelta)[1]<-"SegmentNo"	
		pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		tasc[which(tasc>=1)] = 1
		image(base.asc,ann=F,axes=F,col='white')
		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		zlim=c(0,1)
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
		plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
		labs=c("0","1")
		if (tax==c("frog")) color.legend(118,-42,140,-41,labs,cols,cex=8)
			
}
mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=8,at=seq(1/8,0.99,1/4))
mtext(c('Immigrants','Emmigrants'),side=3,line=1,outer=TRUE,cex=8,at=seq(0.25,0.99,1/2))
dev.off()

