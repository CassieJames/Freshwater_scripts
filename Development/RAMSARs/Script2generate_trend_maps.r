#Script to generate trend maps for each RAMSAR
# qsub -l nodes=2 -l pmem=5gb -I
# Code to produce trend graphs for ramsars
#required to run ... module load R-2.15.1
library(SDMTools);library(maptools); library(plotrix)
source('/home/jc148322/scripts/libraries/cool_functions.r') 

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz');
baseasc=base.asc
pos = as.data.frame(which(is.finite(base.asc),arr.ind=T))
pos$lat = getXYcoords(base.asc)$y[pos$col]
pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon
ESs=c('RCP3PD','RCP45','RCP6','RCP85') 
YEARs=seq(2015,2085,10) 
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/tmp/"
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/RAMSAR_trend_figures/"

RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load ramsar  asc
RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')
pos$RAMSAR  = extract.data(cbind(pos$lon,pos$lat), RAMasc)    
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector


### set up data frame to create names and zoom extent for each ramsar
ram= c(13,27,26,32,31,33,9,15,17,14,12,6,1,3,2,5,24,4,29,22,28,36,37)
REFCODE=c(64,56,55,39,38,36,24,52,28,65,53,50,43,41,51,44,34,42,33,32,31,2,1)
zoom=c(20,100,150,150,100,50,200,15,15,15,200,20,50,20,20,20,20,20,20,50,30,30,30)
refcode_names=data.frame(ram,REFCODE,zoom)
refcode_names2=merge(refcode_names,RAMinfo[,c(2,3)], by='REFCODE')


###################################################################################################
# Create summary for annual deltas and sd for each RAMSAR

out=NULL 
files=list.files(data.dir, pattern=RCP85)	
	for (ii in 1:length(files)) {
		
		load(paste(data.dir,files[ii], sep=""))#read in data
		outdelta=cbind(pos[,1:4],outdelta)
		
		for(ram in RAMSARS) {
				
				tdata = outdelta[which(pos$RAMSAR==ram),] #get the data only for the RAMSAR of interest	
				out2 = colMeans(tdata[,5:10])
				out = rbind(out, data.frame(files=gsub("_percentiles.Rdata","",files[ii]),RAMSARS=ram,Delta_10=out2[1],Delta_50=out2[2],Delta_90=out2[3],
				SD_10=out2[4],SD_50=out2[5],SD_90=out2[6]))		

		
		}		
	}	
###################################################################################################
# Script to extract absolute annual values for figures
voi=c("tmp")
load(paste('/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/',voi,'.Rdata',sep='')) 		

out=NULL	
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector
RAMSARS=RAMSARS[-c(5,12)] # remove these two temporarily (6 and 31) as they cause problems due to the fact they re extremely small and therefore only present in 1 grid square
	
	for(ram in RAMSARS) {
		
		tdata = annualdata[which(pos$RAMSAR==ram),] 
		
		for(es in ESs) {
			
			temp=tdata[,grep(es,colnames(tdata))]
			
			for(year in YEARs) {
			
				ttemp=temp[,grep(year,colnames(temp))]
				outquant = t(apply(ttemp,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out2 = colMeans(outquant)
				out = rbind(out, data.frame(RAMSARS=ram,ESs=es, YEARs=year, Q10th=out2[1],Q50th=out2[2],Q90th=out2[3]))		
			}	
		}
	}



	for (ram in RAMSARS) {		
			
			ram_name=refcode_names2$RAMSAR_NAM[which(refcode_names2$ram==ram)] [1]
			png(paste(image.dir,ram_name,'_pre_trends.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1], units='px', pointsize=20, bg='white') 
			par(mfrow=c(1,2),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85") |(out$RAMSARS==ram) &(out$ESs=="RCP45") ,]	
			ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP45"),]	
			#ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Annual precipitation (mm)', font.sub=2, font.lab=3, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.2, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey20')
			assign.list(l,r,b,t) %=% par("usr") 	
			rect(l, b, r, t, col = "grey90",lty=0) 	
			grid(nx=7,ny=ylim[2]-ylim[1],col='white', lty=1,lwd=0.5)
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs,labels=YEARs,lwd=0.5,lwd.ticks=0.5,cex=1.5,col='grey20') 	
			axis(2,seq(ylim[1],ylim[2],1),labels=round(seq(ylim[1],ylim[2],1)),lwd=0.5,lwd.ticks=0.5,cex=1.5,col='grey20') 	 	
			legend(2016,ylim[1]+40, 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0) 	
			legend(2018,ylim[1]+60, 'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n') 	
			mtext('Low (RCP45)', line=3,  side=1, cex=1.5,font=2)  	
			mtext(ram_name,line=1,  side=3, cex=1.5,font=1.5)  
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85"),]	
			#ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Annual precipitation (mm)', font.sub=2, font.lab=3, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.2, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey20')
			assign.list(l,r,b,t) %=% par("usr") 	
			rect(l, b, r, t, col = "grey90",lty=0) 	
			grid(nx=7,ny=ylim[2]-ylim[1],col='white', lty=1,lwd=0.5)
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs,labels=YEARs,lwd=0.5,lwd.ticks=0.5,cex=1.5,col='grey20') 	
			axis(2,seq(ylim[1],ylim[2],1),labels=round(seq(ylim[1],ylim[2],1)),lwd=0.5,lwd.ticks=0.5,cex=1.5,col='grey20') 	 	
			legend(2016,ylim[1]+40, 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0) 	
			legend(2018,ylim[1]+60,'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n') 	
			mtext('High (RCP85)', line=3,  side=1, cex=1.5,font=2)  
			
			dev.off() 
	}








































