###################################################################################################
### scripts to prepare figures for final report


module load R-2.15.1

#Load R
#create some images

library(plotrix) 
library(SDMTools)
library(maptools)

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') 
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) 
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)#colorRampPalette(c('red4','orangered','gold','beige','tan','lightblue4','blue4'))(21) #define a set of colors
cols = all.cols[21:1] # blue to red
image.dir = "/home/jc246980/Final report/Figures/"
data.dir="/home/jc246980/DrySeason/Futuredat/Quantiles_data/"
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))
ESs = list.files(futdir, pattern="RCP") 	
es=ESs[4]
year=2085

###################################################################################################
### Dry season figures - future
VOIS = c('num_month', 'max_clust_length', 'fut_month_max_clust')

					
				png(paste(image.dir,'Dryseason_data_',year,'.png',sep=''),width=dim(base.asc)[1]*2, height=dim(base.asc)[2]*2+100, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(3,3),cex=1,oma=c(1,3,3,0)) #define the plot parameters
					first=TRUE
					for (voi_delta in VOIS) { cat(voi_delta,'\n')	
						load(paste(data.dir,voi_delta,"_data.Rdata",sep='')) #loads dry season metrics
						deltalim_x=round(range(outdata)[1],1)
						deltalim_y=round(range(outdata)[2],1)
						deltalims=c(deltalim_x,deltalim_y)
						deltamid=round(((deltalims[1]+deltalims[2])/2),1)
						deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
						
						if(voi_delta=='total_severity' | voi_delta=='fut_clust_severity'){
						
							for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,paste(es,year,percentile,sep='_')] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
									if (percentile==10) legend.gradient(pnts,cols=all.cols,limits=deltalims, title='')
							}
						}else{
						
							for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,paste(es,year,percentile,sep='_')] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
									if (percentile==10) legend.gradient(pnts,cols=cols,limits=deltalims, title='')
							}
						}
						
						mtext(c('10th Percentile','50th Percentile','90th Percentile'),side=3,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
						mtext(c('Cluster Start','Cluster Length','Total Length'),side=2,line=1,outer=TRUE,cex=2, at=seq(1/6, 0.99, 1/3))
					}
				dev.off() #close out the image
				
				
#### Current dry season images for means			
				
VOIS=list.files('/home/jc246980/DrySeason/Currentdat/')
VOIS=c(VOIS[4], VOIS[2], VOIS[3])
data.dir='/home/jc246980/DrySeason/Currentdat/'
	png(paste(image.dir,'Dryseason_current_data.png',sep=''),width=dim(base.asc)[1]*2, height=dim(base.asc)[2]*2+100, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(2,2),cex=1,oma=c(1,3,3,0)) #define the plot parameters
					first=TRUE
					for (voi_delta in VOIS) { cat(voi_delta,'\n')	
						outdata=load(paste(data.dir,voi_delta,sep='')) #loads dry season metrics
						outdata=get(outdata)
						deltalim_x=round(range(outdata[,31])[1],1)
						deltalim_y=round(range(outdata[,31])[2],1)
						deltalims=c(deltalim_x,deltalim_y)
						deltamid=round(((deltalims[1]+deltalims[2])/2),1)
						deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
						
						if(voi_delta=='DrySeason_total.severity.Rdata' | voi_delta=='DrySeason_clust.severity.Rdata'){
						
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,31] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
									if(voi_delta=="DrySeason_total.severity.Rdata") {
									legend.gradient(pnts,cols=all.cols,limits=deltalims, title='Total Severity', cex=1.5)}  	
									if(voi_delta=="DrySeason_clust.severity.Rdata") {
									legend.gradient(pnts,cols=all.cols,limits=deltalims, title='Cluster Severity', cex=1.5)}
								
						
						}else{
						
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,31] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
									if(voi_delta=="DrySeason_max.clust.length.Rdata") {  
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Cluster length', cex=1.5)}
									if(voi_delta=="DrySeason_num.month.Rdata") { 
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Total Length', cex=1.5)}
									if(voi_delta=="DrySeason_month.max.clust.Rdata") {
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Cluster Start', cex=1.5)}									
									
							}
											
					}
				dev.off() #close out the image
				
VOIS=list.files('/home/jc246980/DrySeason/Currentdat/')
VOIS=c(VOIS[4], VOIS[5], VOIS[2], VOIS[1], VOIS[3])
data.dir='/home/jc246980/DrySeason/Currentdat/'
	png(paste(image.dir,'Dryseason_current_data_sd.png',sep=''),width=dim(base.asc)[1]*2, height=dim(base.asc)[2]*2+100, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(2,2),cex=1,oma=c(1,3,3,0)) #define the plot parameters
					first=TRUE
					for (voi_delta in VOIS) { cat(voi_delta,'\n')	
						outdata=load(paste(data.dir,voi_delta,sep='')) #loads dry season metrics
						outdata=get(outdata)
						deltalim_x=round(range(outdata[,32])[1],1)
						deltalim_y=round(range(outdata[,32])[2],1)
						deltalims=c(deltalim_x,deltalim_y)
						deltamid=round(((deltalims[1]+deltalims[2])/2),1)
						deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
						
						if(voi_delta=='DrySeason_total.severity.Rdata' | voi_delta=='DrySeason_clust.severity.Rdata'){
						
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,32] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=all.cols) #create the image
									if(voi_delta=="DrySeason_total.severity.Rdata") {
									legend.gradient(pnts,cols=all.cols,limits=deltalims, title='Total Severity', cex=1.5)}  	
									if(voi_delta=="DrySeason_clust.severity.Rdata") {
									legend.gradient(pnts,cols=all.cols,limits=deltalims, title='Cluster Severity', cex=1.5)}
								
						
						}else{
						
									tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdata[,32] #get the data
									tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
									image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
									if(voi_delta=="DrySeason_max.clust.length.Rdata") {  
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Cluster length', cex=1.5)}
									if(voi_delta=="DrySeason_num.month.Rdata") { 
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Total Length', cex=1.5)}
									if(voi_delta=="DrySeason_month.max.clust.Rdata") {
									legend.gradient(pnts,cols=cols,limits=deltalims, title='Cluster Start', cex=1.5)}									
									
							}
											
					}
				dev.off() #close out the image
				




























