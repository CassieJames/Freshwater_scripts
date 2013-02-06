###############################################################################
# Scripts to create dry season change (deltas and sds) images
# C James ...................9th January 2012  
############################################################################### 

module load R-2.15.1

#Load R
#create some images

library(plotrix) 
library(SDMTools)

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') 
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) 
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)#colorRampPalette(c('red4','orangered','gold','beige','tan','lightblue4','blue4'))(21) #define a set of colors
cols = all.cols[21:1] # blue to red
legend.pnts = cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38)) #define the location of the legend
image.dir = "/home/jc246980/Hydrology.trials/Stability/Runoff/Images/"
data.dir="/home/jc246980/Hydrology.trials/Stability/Runoff/Quantiles_delta/"

futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
ESs = list.files(futdir, pattern="RCP") 	
YOIS=seq(2015,2085,10)


	

	 for (year in YOIS) { cat(year,'\n') #cycle through each of the years
				
			png(paste(image.dir,'Runoff_delta_',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
				par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
				first=TRUE
									
				load(paste(data.dir,"Runoff_delta.Rdata",sep='')) #loads runoff delta data
				deltalim_x=round(range(outdelta)[1],1)
				deltalim_y=round(range(outdelta)[2],1)
				deltalims=c(deltalim_x,deltalim_y)
				deltalabs = c(paste('<',deltalims[1]),2.5,paste('>',deltalims[2]))
				
				for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
					for (es in ESs) { cat(es,'\n') #cycle through the emission scenarios of interst
						##plot the delta
						tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,year,percentile,sep='_')] #get the data
						tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
						image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
						if (percentile==90 & es==ESs[length(ESs)]) color.legend(118,-44,140,-41,deltalabs,cols,cex=2)
					}
				}
				mtext(ESs,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
				mtext(paste(year,'-- Proportionate change in Runoff'),side=1,line=1,outer=TRUE,cex=3)
				mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
			dev.off() #close out the image
		}
	
	
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	