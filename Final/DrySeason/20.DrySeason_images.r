# C james ...................30th July 2012  
############################################################################### 
#create some images
module load R-2.15.1
library(plotrix) 
library(SDMTools)




base.asc = read.asc.gz('base.asc.gz') #read in the base asc file
pos = read.csv('base.positions.csv',as.is=TRUE)
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)#colorRampPalette(c('red4','orangered','gold','beige','tan','lightblue4','blue4'))(21) #define a set of colors
legend.pnts = cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38)) #define the location of the legend
image.dir = "/home/jc246980/DrySeason/Images/" 

#first work out sensible ranges for the figures - best to check 2085 as these will show greatest change relative to current

range(outdelta)
quantile(outdelta,c(0.01,0.5,0.99))
		
deltalims = c(0,5); #define the delta limits
deltalabs = c(paste('<',deltalims[1]),2.5,paste('>',deltalims[2]))
cols = all.cols[21:1] # blue to red
	
	


 for (year in YOIS) { cat(year,'\n') #cycle through each of the years
		##first work with RCPs
		ESoi = ESs[grep('RCP',ESs)] #define the emission scenarios of interest
		png(paste(image.dir,vois_delta[ii]'.',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
			par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
			first=TRUE
			for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
				for (es in ESoi) { cat(es,'\n') #cycle through the emission scenarios of interst
					##plot the delta
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,year,percentile,sep='_')] #get the data
					tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
					if (percentile==90 & es==ESoi[length(ESoi)]) color.legend(118,-44,140,-41,deltalabs,cols,cex=2)
				}
			}
			mtext(ESoi,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
			if (voi_delta =='delta.num.month') {
				mtext(paste(year,'Change in length of dry season'),side=1,line=1,outer=TRUE,cex=3)
			} 
			if (voi_delta =='delta.total.severity') {
				mtext(paste(year,'Proportionate change in total dry season severity'),side=1,line=1,outer=TRUE,cex=3)
			} 
			if (voi_delta =='delta.max.clust.length') {
				mtext(paste(year,'Change in length of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
			} 
			if (voi_delta =='delta.clust.severity') {
				mtext(paste(year,'Proportionate change in severity of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
			} 

      else {
				mtext(paste(year,'Change month when longest consecutive dry period began'),side=1,line=1,outer=TRUE,cex=3)
			}
			mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
		dev.off() #close out the image
	}
