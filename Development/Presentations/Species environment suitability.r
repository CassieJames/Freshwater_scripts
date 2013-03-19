source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
out.dir=paste('/home/jc246980/Presentations/Freshwater_NARP_roadshow/',sep='');setwd(out.dir)


##01. read in layers and create positions file
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos #save a copy of pos
base.asc[which(is.finite(base.asc))]=0

Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

##02. set variables and limits for image
cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
zlim=c(0,2)
vois=c(10,50,90)
taxa=c('fish','crayfish','frog','turtles')
yr=2085	
es='RCP85'

### Create species deltas and save out to file

data.dir="/home/jc246980/SDM/Species_delta_data/"	
		for (tax in taxa) { cat('calculating deltas for ',tax,'\n')

				pos=tpos
				load(paste('/home/jc148322/NARPfreshwater/SDM/richness/',tax,'/',es,'_richness.Rdata',sep=''))

				outdelta=outquant[,3:ncol(outquant)]# make a copy

				outdelta=outdelta/outquant[,2]
				outdelta[which(is.nan(outdelta))]=NA
				outdelta[which(outdelta>2)]=2
				outdelta=cbind(outquant[,1:2],outdelta)
				pos=merge(pos,outdelta, by='SegmentNo',all.x=TRUE)
				write.csv(pos,paste(data.dir,"Delta_", tax,".csv",sep=''),row.names=F)	
		}
	
	
### Create image
	png(paste('species_2085_50th_percentile.png',sep=''),width=dim(base.asc)[1]*1+160, height=dim(base.asc)[2]*1, units='px', pointsize=100, bg='lightgrey')
		
		##set up image layout
		par(mar=c(4,0,0,0),cex=1,oma=c(0,0,0,0)) #define the plot parameters
		mat = matrix(c( 1,1,1,1,1,2,2,2,2,2,
						1,1,1,1,1,2,2,2,2,2,
						1,1,1,1,1,2,2,2,2,2,
						1,1,1,1,1,2,2,2,2,2,
						3,3,3,3,3,4,4,4,4,4,
						3,3,3,3,3,4,4,4,4,4,
						3,3,3,3,3,4,4,4,4,4,
						3,3,3,3,3,4,4,4,4,4),nr=8,nc=10,byrow=TRUE) #create a layout matrix for images
			 #create a layout matrix for images		
		layout(mat) #call layout as defined above
							
		
		voi=50
		pos_fish=read.csv("/home/jc246980/SDM/Species_delta_data/Delta_fish.csv")
		tasc=make.asc(pos_fish[,paste("X",yr,'_',voi,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=5, ann=FALSE,axes=FALSE, add=TRUE)
		
		pos_crayfish=read.csv("/home/jc246980/SDM/Species_delta_data/Delta_crayfish.csv")
		tasc=make.asc(pos_crayfish[,paste("X",yr,'_',voi,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=5, ann=FALSE,axes=FALSE, add=TRUE)
		
		pos_frog=read.csv("/home/jc246980/SDM/Species_delta_data/Delta_frog.csv")
		tasc=make.asc(pos_frog[,paste("X",yr,'_',voi,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=5, ann=FALSE,axes=FALSE, add=TRUE)
		
		pos_turtles=read.csv("/home/jc246980/SDM/Species_delta_data/Delta_turtles.csv")
		tasc=make.asc(pos_turtles[,paste("X",yr,'_',voi,sep='')])
		image(base.asc,ann=F,axes=F,col='white')
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim,add=TRUE)
		plot(Drainageshape , lwd=5, ann=FALSE,axes=FALSE, add=TRUE)
			

		#legend and labels
		labs=c("0",".2",".4",".6",".8","1","1.2","1.4","1.6","1.8",">=2")
		plot(1:20,axes=FALSE,ann=FALSE,type='n')
		text(10,14,"Proportion of current",cex=2)
		color.legend(2,10,18,12,labs,rect.col=cols,align="rb",gradient="x", cex=1)
		legend(1.3,8, fill='white','No richness, current or future',bty='n', cex=1.5)

	dev.off()
		


