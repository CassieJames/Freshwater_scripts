#Script to climate space for all RAMSARs
# qsub -l nodes=2 -l pmem=5gb -I
# Code to produce trend graphs for ramsars
#required to run ... module load R-2.15.1
library(SDMTools);library(maptools); library(plotrix); library(shape)
source('/home/jc148322/scripts/libraries/cool_functions.r') 

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz');
baseasc=base.asc
pos = as.data.frame(which(is.finite(base.asc),arr.ind=T))
pos$lat = getXYcoords(base.asc)$y[pos$col]
pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon
ESs=c('RCP3PD','RCP45','RCP6','RCP85') 
YEARs=seq(2015,2085,10) 
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/RAMSAR_dryseason_figures/"

RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load ramsar asc
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
###Script to sort means and sd for current for each ramsar and determine an average 

vois_files=list.files("/home/jc246980/DrySeason/Currentdat/")
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector
RAMSARS=RAMSARS[-c(5,12)] # remove these two temporarily (6 and 31) as they cause problems due to the fact they are extremely small and therefore only present in 1 grid square
	for (voi in vois_files) {
		out=NULL
		tdata=load(paste('/home/jc246980/DrySeason/Currentdat/',voi,sep='')) 		
		tdata=get(tdata)
		
		for(ram in RAMSARS) {
			
			temp = tdata[which(pos$RAMSAR==ram),] 
			out2=colMeans(temp[,c(31:32)])
			out = rbind(out, data.frame(RAMSARS=ram,mean=out2[1],sd=out2[2]))	
		
		}
	
	write.csv(out,paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", voi,".csv", sep = ''), row.names = F )
	}

###################################################################################################
###Script to determine percentiles for absolute values for futures
vois_files=c("num_month.Rdata","total_severity.Rdata","max_clust_length.Rdata", "fut_clust_severity.Rdata","fut_month_max_clust.Rdata")
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector
RAMSARS=RAMSARS[-c(5,12)] # remove these two temporarily (6 and 31) as they cause problems due to the fact they are extremely small and therefore only present in 1 grid square

for(voi in vois_files) {
	out=NULL
	for (es in ESs) {	
		
		tdata=load(paste('/home/jc246980/DrySeason/Futuredat/Data/',es,"_",voi,sep='')) 		
		tdata=get(tdata)	

		for(ram in RAMSARS) {
			
			temp = tdata[which(pos$RAMSAR==ram),] 
			
			for(yy in YEARs) {
				
				temp2=temp[,grep(yy,colnames(temp))]
				outquant = t(apply(temp2,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out2 = colMeans(outquant)
				out = rbind(out, data.frame(ESs=es,RAMSARS=ram, YEARs=yy, Q10th=out2[1],Q50th=out2[2],Q90th=out2[3]))		
			
			}	
		}
	}
	write.csv(out,paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", voi,".csv", sep = ''), row.names = F )
}
	

###################################################################################################
###Script to map shifts in climate space with respect to changes in dry season

vois_files=list.files("/home/jc246980/DrySeason/Currentdat/")
		current_severity=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", vois_files[5],".csv", sep = ''))
		current_num_mth=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", vois_files[4],".csv", sep = ''))
vois_files=c("num_month.Rdata","total_severity.Rdata","max_clust_length.Rdata", "fut_clust_severity.Rdata","fut_month_max_clust.Rdata")		
		future_severity=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", vois_files[2],".csv", sep = ''))
		future_num_mth=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", vois_files[1],".csv", sep = ''))
		
		for (es in ESs) {
		
		
			for (yy in YEARs) {
			
				future_date_severity=future_severity[which((future_severity$YEARs==yy) & (future_severity$ESs==es)),]
				future_date_num_mth=future_num_mth[which((future_num_mth$YEARs==yy) & (future_num_mth$ESs==es)),]
				
				X_ellipse=as.data.frame((future_date_severity[,4]+future_date_severity[,6])/2)
				Y_ellipse=as.data.frame((future_date_num_mth[,4]+future_date_num_mth[,6])/2)
				X_radius= abs((future_date_severity[,6]-future_date$X_ellipse))
				Y_radius=abs((future_date_num_mth[,6]-future_date$Y_ellipse))
				future_date=cbind(X_ellipse,Y_ellipse,X_radius,Y_radius)
				colnames(future_date) =c("X_ellipse","Y_ellipse", "X_radius", "Y_radius") 
			
			png(paste(image.dir,es,"_",yy,"SeverityVSNoMth.png",sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=50, bg='#F0F8FF') 
			par(mfrow=c(1,1),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
			
			ylim=c(round(min(future_date_num_mth[,4:6])-0.5),round(max(future_date_num_mth[,4:6])+0.5))
			xlim=c(round(min(future_date_severity[,4:6])-50),round(max(future_date_severity[,4:6])+50))
			
			plot(rbind(future_date_severity[,5], current_severity[,2]) ,rbind(future_date_num_mth[,5], current_num_mth[,2]) ,xlim=xlim, ylim=ylim,xlab='Dry Season Severity', ylab='Number of months', 
			font.sub=2, font.lab=3,col = greycol(100), cex.lab=1.2, cex.axis=1, axes=T,xaxs='i',yaxs='i', col.axis='black', pch=20)	
			
			for(i in 1:nrow(future_date)){
				filledellipse(future_date$X_radius[i], ry1 = future_date$Y_radius[i], mid = c(future_date$X_ellipse[i],future_date$Y_ellipse[i]),col="#698B2255",lty=3, lwd=2)
			}
			
			for(i in 1:nrow(current_severity)){
					arrows(current_severity[,2], current_num_mth[,2], future_date_severity[,5], future_date_num_mth[,5], length = 0.25, angle = 30, lwd=4)
			}
			
			
			
			dev.off() 
			
			}
		
		}
	

			
			
			
			
	
	
	
	
	