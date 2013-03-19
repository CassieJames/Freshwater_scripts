###################################################################################################
### scripts to prepare figures for final report


module load R-2.15.1

#Load R
#create some images

library(plotrix) 
library(SDMTools)
library(maptools)

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

data.dir="/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/TomHarwood_data/"
image.dir="/home/jc246980/Final report/Figures/"
load(paste(data.dir,'Qrun.current_5km_means.Rdata',sep=''))

Qrun_annuals = apply(Qrun,1,sum)
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)

png(paste(image.dir,'Current_runoff.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='lightgrey')
		par(mar=c(0,0,0,0),cex=1,oma=c(3,3,3,0)) #define the plot parameters
		mat = matrix(c( 1,1,1,1,1,1,
							1,1,1,1,1,1,
							1,1,1,1,1,1,
							1,1,1,1,1,1,
							1,1,1,1,1,1,
							1,1,1,1,1,1),nr=7,nc=6,byrow=TRUE) #create a layout matrix for images
		
			layout(mat) #call layout as defined above  



	    Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun_annuals
		Qrun.asc[which(Qrun.asc==0)] = 0
		Qrun.asc[which(Qrun.asc>0 & Qrun.asc<5)] = 1
		Qrun.asc[which(Qrun.asc>=5 & Qrun.asc<10)] = 2
		Qrun.asc[which(Qrun.asc>=10 & Qrun.asc<25)] = 3
		Qrun.asc[which(Qrun.asc>=25 & Qrun.asc<50)] = 4
		Qrun.asc[which(Qrun.asc>=50 & Qrun.asc<75)] = 5
		Qrun.asc[which(Qrun.asc>=75 & Qrun.asc<100)] = 6
		Qrun.asc[which(Qrun.asc>=100 & Qrun.asc<200)] = 7
		Qrun.asc[which(Qrun.asc>=200 & Qrun.asc<300)] = 8
		Qrun.asc[which(Qrun.asc>=300 & Qrun.asc<500)] = 9
		Qrun.asc[which(Qrun.asc>=500 & Qrun.asc<1000)] = 10
		Qrun.asc[which(Qrun.asc>=1000 & Qrun.asc<3500)] = 11
		Qrun.asc[which(Qrun.asc>=3500)] = 12
		zlims = range(c(0,as.vector(Qrun.asc)),na.rm=TRUE)
		image(Qrun.asc, ann=FALSE,axes=FALSE,col=all.cols,zlim=zlims)
		text(130,-39.5,"Mean Annual Runoff (mm)",cex=4)
		labs=c(0,5,10,25,50,75,100,200,300,500,1000,3500)
		color.legend(114,-42.5,144,-40.5,labs,rect.col=all.cols,align="rb",gradient="x", cex=2)


dev.off()