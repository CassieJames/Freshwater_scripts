###################################################################################################
#### Script to create images
qsub -l nodes=2 -l pmem=5gb -I
module load R-2.15.1
### Start R

library(SDMTools); library(maptools); library(plotrix) 
source('/home/jc148322/scripts/libraries/cool_functions.r')

data.dir="/home/jc246980/Hydrology.trials/Accumulated_reach/"
image.dir = "/home/jc246980/Final report/Figures/"

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
base.asc = read.asc.gz('base.asc.gz');

raster=read.asc.gz('/home/jc246980/Janet_Stein_data/Catchmentraster1km.asc')
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]    
   
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile			

# outdelta=read.csv(paste(data.dir,"Accumulated_runoff_delta.csv",sep=''))	
# save(outdelta,file=paste(data.dir,"Accumulated_runoff_delta.Rdata",sep=''))
#colnames(outdelta)[1]="SegmentNo"
pos=merge(pos,outdelta,by='SegmentNo',all.x=TRUE)


all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)#colorRampPalette(c('red4','orangered','gold','beige','tan','lightblue4','blue4'))(21) #

ESs=c('RCP3PD','RCP45','RCP6','RCP85') 
es=ESs[4]
YEARs=seq(2015,2085,10) 
year=2085

	png(paste(image.dir,'Accumulated_runoff_delta_',year,'.png',sep=''),width=dim(base.asc)[1]*1+80, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='lightgrey')
		par(mar=c(0,0,0,0),cex=1,oma=c(4,4,4,0)) #define the plot parameters

	    mat = matrix(c( 1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					2,2,2,2,2,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					3,3,3,3,3,
					4,4,4,4,4),nr=13,nc=5,byrow=TRUE) #create a layout matrix for images
		
				layout(mat) #call layout as defined above
				percentile=50
				outdeltalims=outdelta[,paste(es,year,percentile,sep='_')]
				deltalim_x=round(range(outdeltalims)[1],1)
				deltalim_y=round(range(outdeltalims)[2],1)
				deltalims=c(deltalim_x,deltalim_y)
				deltamid=round(((deltalims[1]+deltalims[2])/2),1)
				deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
				
				for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
					tasc=base.asc; tasc[cbind(pos250$row,pos250$col)]=pos[,paste(es,year,percentile,sep='_')]
					tasc[which(tasc==0)] = 0
					tasc[which(tasc>0 & tasc<.25)] = -10
					tasc[which(tasc>=.25 & tasc<0.5)] = -9
					tasc[which(tasc>=0.5 & tasc<0.75)] = -8
					tasc[which(tasc>=0.75 & tasc<1)] = -7
					tasc[which(tasc>=1 & tasc<2)] = -6
					tasc[which(tasc>=2 & tasc<5)] = -5
					tasc[which(tasc>=5 & tasc<10)] = -4
					tasc[which(tasc>=10 & tasc<50)] = -3
					tasc[which(tasc>=50 & tasc<100)] = -2
					#tasc[which(tasc>=100 & tasc<1000)] = -10
					tasc[which(tasc>=100)] = -1
					zlims = range(c(0,as.vector(tasc)),na.rm=TRUE)
					image(tasc, ann=FALSE,axes=FALSE,col=all.cols,zlim=zlims)
					plot(Drainageshape , lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				}

				  labs=c("<.25",".25",".5",".75","1","2","5","10","50",">100")
				  plot(1:20,axes=FALSE,ann=FALSE,type='n')
				  text(10,18,"Difference from current",cex=3)
				  color.legend(2,10,18,15,labs,rect.col=all.cols,align="rb",gradient="x", cex=1.5)
				  mtext(c('10th Percentile','50th Percentile','90th Percentile'),side=2,line=1,outer=TRUE,cex=2.5,at=c(0.25, 0.55, 0.86))

		dev.off() #close out the
		
		
		# zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(c(pos$flow,pos$jflow),na.rm=T))
# # zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(tasc,na.rm=T))
# cols = colorRampPalette(c('tan',"#E0F3F8","#ABD9E9","#90c4de","#74ADD1","#5d91c3","#4575B4","#4067ad","#3c58a6","#37489d","#313695","#191c90",'blue4','#0d0d7d','midnightblue','#131354','#0d0d39','#06071e'))(100)

# pnts=cbind(x=c(146.5,148,148,146.5),y=c(-12,-12,-16,-16))


	# image(tasc, ann=F,axes=F,zlim=zlim,col=cols)
	# mtext('VanDerWal et al 2013', side=1, cex=15)
	# image(jasc, ann=F,axes=F,zlim=zlim,col=cols)

	# mtext('Stein 2012', side=1,cex=15)
	# legend.gradient(pnts,col=cols, round(zlim/1000), title='Flow (GL)',cex=12)