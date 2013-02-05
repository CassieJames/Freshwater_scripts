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
image.dir = "/home/jc246980/DrySeason/Images/" 
data.dir="/home/jc246980/DrySeason/Futuredat/Quantiles_delta/"
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	

ESs = list.files(futdir, pattern="RCP") 	
YOIS=seq(2015,2085,10)
VOIS = c('delta_num_month', 'delta_total_severity', 'delta_max_clust_length', 'delta_fut_clust_severity', 'delta_month_max_clust')


	
	for (voi_delta in VOIS) { cat(voi_delta,'\n')	

		 for (year in YOIS) { cat(year,'\n') #cycle through each of the years
					
				png(paste(image.dir,voi_delta,'.',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
					first=TRUE
										
					load(paste(data.dir,voi_delta,"_delta.Rdata",sep='')) #loads dry season metrics
					deltalim_x=round(range(outdelta)[1],1)
					deltalim_y=round(range(outdelta)[2],1)
					deltalims=c(deltalim_x,deltalim_y)
					deltamid=round(((deltalims[1]+deltalims[2])/2),1)
					deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
					
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
					if (voi_delta =='delta_num_month') {
						mtext(paste(year,'-- Change in length of dry season'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_delta =='delta_total_severity') {
						mtext(paste(year,'-- Proportionate change in total dry season severity'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_delta =='delta_max_clust_length') {
						mtext(paste(year,'-- Change in length of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_delta =='delta_fut_clust_severity') {
						mtext(paste(year,'-- Proportionate change in severity of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					}
					if (voi_delta =='delta_month_max_clust') {
						mtext(paste(year,'-- Change month when longest consecutive dry period began'),side=1,line=1,outer=TRUE,cex=3)
					}
					mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
				dev.off() #close out the image
			}
	}
	
	
	
	
data.dir="/home/jc246980/DrySeason/Futuredat/Quantiles_sd/"
VOIS = c('sd_num_month', 'sd_total_severity', 'sd_max_clust_length', 'sd_fut_clust_severity', 'sd_month_max_clust')

	for (voi_sd in VOIS) { cat(voi_sd,'\n')	

		 for (year in YOIS) { cat(year,'\n') #cycle through each of the years
					
				png(paste(image.dir,voi_sd,'.',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
					first=TRUE
										
					load(paste(data.dir,voi_sd,"_sd.Rdata",sep='')) #loads dry season metrics
					sdlim_x=round(range(outsd)[1],1)
					sdlim_y=round(range(outsd)[2],1)
					sdlims=c(sdlim_x,sdlim_y)
					sdmid=round(((sdlims[1]+sdlims[2])/2),1)
					sdlabs = c(paste('<',sdlims[1]),sdmid,paste('>',sdlims[2]))
					
					for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
						for (es in ESs) { cat(es,'\n') #cycle through the emission scenarios of interst
							##plot the delta
							tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outsd[,paste(es,year,percentile,sep='_')] #get the data
							tasc[which(tasc<sdlims[1])] = sdlims[1]; tasc[which(tasc>sdlims[2])] = sdlims[2] #ensure all data within limits
							image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols) #create the image
							if (percentile==90 & es==ESs[length(ESs)]) color.legend(118,-44,140,-41,sdlabs,cols,cex=2)
						}
					}
					mtext(ESs,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
					if (voi_sd =='sd_num_month') {
						mtext(paste(year,'-- Number of sd from current length of dry season'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_sd =='sd_total_severity') {
						mtext(paste(year,'-- Number of sd from current total dry season severity'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_sd =='sd_max_clust_length') {
						mtext(paste(year,'-- Number of sd from current longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_sd =='sd_fut_clust_severity') {
						mtext(paste(year,'-- Number of sd from current severity of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					}
					if (voi_sd =='sd_month_max_clust') {
						mtext(paste(year,'-- Number of sd from current month when longest consecutive dry period began'),side=1,line=1,outer=TRUE,cex=3)
					}
					mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
				dev.off() #close out the image
			}
	}
	
	
	
data.dir="/home/jc246980/DrySeason/Futuredat/Quantiles_data/"
VOIS = c('num_month', 'total_severity', 'max_clust_length', 'fut_clust_severity', 'fut_month_max_clust')

	for (voi_data in VOIS) { cat(voi_data,'\n')	

		 for (year in YOIS) { cat(year,'\n') #cycle through each of the years
					
				png(paste(image.dir,voi_data,'.',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
					first=TRUE
										
					load(paste(data.dir,voi_data,"_data.Rdata",sep='')) #loads dry season metrics
					datalim_x=round(range(outdata)[1],1)
					datalim_y=round(range(outdata)[2],1)
					datalims=c(datalim_x,datalim_y)
					datamid=round(((datalims[1]+datalims[2])/2),1)
					datalabs = c(paste('<',datalims[1]),datamid,paste('>',datalims[2]))
					
					for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
						for (es in ESs) { cat(es,'\n') #cycle through the emission scenarios of interst
							##plot the delta
							tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,paste(es,year,percentile,sep='_')] #get the data
							tasc[which(tasc<datalims[1])] = datalims[1]; tasc[which(tasc>datalims[2])] = datalims[2] #ensure all data within limits
							image(tasc,ann=FALSE,axes=FALSE,zlim=datalims,col=cols) #create the image
							if (percentile==90 & es==ESs[length(ESs)]) color.legend(118,-44,140,-41,datalabs,cols,cex=2)
						}
					}
					mtext(ESs,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
					if (voi_data =='num_month') {
						mtext(paste(year,'-- Length of dry season'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_data =='total_severity') {
						mtext(paste(year,'-- Total dry season severity'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_data =='max_clust_length') {
						mtext(paste(year,'-- Longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					} 
					if (voi_data =='fut_clust_severity') {
						mtext(paste(year,'-- Severity of longest consecutive dry period'),side=1,line=1,outer=TRUE,cex=3)
					}
					if (voi_data =='fut_month_max_clust') {
						mtext(paste(year,'-- Month when longest consecutive dry period began'),side=1,line=1,outer=TRUE,cex=3)
					}
					mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
				dev.off() #close out the image
			}
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	