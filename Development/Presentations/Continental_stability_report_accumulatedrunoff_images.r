###################################################################################################
#### Script to create images
qsub -l nodes=2 -l pmem=10gb -I
module load R-2.15.1
### Start R

library(SDMTools); library(maptools); library(plotrix) 
source('/home/jc148322/scripts/libraries/cool_functions.r')

data.dir="/home/jc246980/Stability/Output/"
image.dir = "/home/jc246980/Presentations/Freshwater_NARP_roadshow/"

raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz')
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]    
base.asc=raster   
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile			

# outdelta=read.csv(paste(data.dir,"Accumulated_runoff_delta.csv",sep=''))	
#save(outdelta,file=paste(data.dir,"Accumulated_runoff_delta.Rdata",sep=''))
#colnames(outdelta)[1]="SegmentNo"

load(paste(data.dir,"Accumulated_runoff_delta.Rdata",sep=''))
pos=merge(pos,outdelta,by='SegmentNo',all.x=TRUE)

all.cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)



ESs=c('RCP3PD','RCP45','RCP6','RCP85') 
es=ESs[4]
YEARs=seq(2015,2085,10) 
year=2085


deltalims=c(0,2)
deltalabs=c('0','>=2')



	png(paste(image.dir,'Accumulated_runoff_delta_',year,'.png',sep=''),width=dim(base.asc)[1]*1+300, height=dim(base.asc)[1]/2, units='px', pointsize=10, bg='lightgrey')
		par(mar=c(0,4,0,0),cex=1,oma=c(4,4,4,0)) #define the plot parameters


	    mat = matrix(c( 1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,
						1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,
						1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,
						1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,
						5,5,5,5,5,4,4,4,4,4,5,5,5,5,5),nr=5,nc=15,byrow=TRUE) #create a layout matrix for images
		
				layout(mat) #call layout as defined above

				
				for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
					tasc=base.asc; tasc[cbind(pos$row,pos$col)]=pos[,paste(es,year,percentile,sep='_')]
					tasc[which(tasc>2)] = 2
					tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
				    image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
					plot(Drainageshape , lwd=4, ann=FALSE,axes=FALSE, add=TRUE)
				}

				  plot(1:20,axes=FALSE,ann=FALSE,type='n')
				  text(10,18,"Difference from current",cex=12)
				  color.legend(2,10,18,15,deltalabs,rect.col=all.cols,align="rb",gradient="x", cex=6)
				  #mtext(c('90th Percentile','50th Percentile','10th Percentile'),side=3,line=1,outer=TRUE,cex=8,at=c(0.20, 0.55, 0.85))

		dev.off() #close out the
		
	