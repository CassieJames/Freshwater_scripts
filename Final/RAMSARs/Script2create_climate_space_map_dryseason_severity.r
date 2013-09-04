#Script to climate space for all RAMSARs
# qsub -l nodes=2 -l pmem=5gb -I
# Code to produce trend graphs for ramsars
#required to run ... module load R-2.15.1
library(SDMTools);library(maptools); library(plotrix); library(shape)
source('/home/jc148322/scripts/libraries/cool_functions.r') 

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz')
baseasc=base.asc
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244   
ESs=c('RCP3PD','RCP45','RCP6','RCP85') 
YEARs=seq(2015,2085,10) 
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/RAMSAR_dryseason_figures/"

### set up data frame to create names and zoom extent for each ramsar
RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')

ram= c(13,27,26,32,31,33,9,15,17,14,12,6,1,3,2,5,24,4,29,22,28,36,37,7,8,10,11, 16, 23, 25, 30)
refcode=c(64,56,55,39,38,36,24,52,28,65,53,50,43,41,51,44,34,42,33,32,31,2,1,48,62, 49, 47, 23, 54, 35, 37)
zoom=c(20,100,100,100,100,50,150,15,15,15,100,20,50,20,20,20,20,20,50,30,30,30,30,80, 30, 80, 80, 100, 30, 50, 30)
ref_table=cbind(as.data.frame(ram), as.data.frame(refcode),as.data.frame(zoom))
ref_table=merge(ref_table,RAMinfo[, c("REFCODE","RAMSAR_NAM")], by.x='refcode', by.y='REFCODE')
ref_table$RAMSAR_NAM=as.character(ref_table$RAMSAR_NAM)

ref_table$RAMSAR_NAM[ which(ref_table$RAMSAR_NAM=="Toolibin Lake (also known as Lake Toolibin)")] <- "Toolibin Lake"
ref_table$RAMSAR_NAM[ which(ref_table$RAMSAR_NAM=="Currawinya Lakes (Currawinya National Park)")] <- "Currawinya Lakes"
ref_table$RAMSAR_NAM[ which(ref_table$RAMSAR_NAM=="Shoalwater and Corio Bays Area (Shoalwater Bay Training Area, in part - Corio Bay)")] <- "Shoalwater and Corio Bays Area"
ref_table$RAMSAR_NAM[ which(ref_table$RAMSAR_NAM=="Gwydir Wetlands: Gingham and Lower Gwydir (Big Leather) Watercourses")] <- "Gwydir Wetlands"
ref_table$RAMSAR_NAM[ which(ref_table$RAMSAR_NAM=="Great Sandy Strait (including Great Sandy Strait, Tin Can Bay and Tin Can Inlet).")] <- "Great Sandy Strait"

 
wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_5km.Rdata',sep=''))
RAMSARS=unique(Ramsar_area_agg$ramsar)

###################################################################################################
###Script to determine percentiles for absolute values for futures
vois_files=c("num_month.Rdata","total_severity.Rdata","max_clust_length.Rdata", "fut_clust_severity.Rdata","fut_month_max_clust.Rdata")


for(voi in vois_files) {
	out=NULL
	for (es in ESs) {	
		
		tdata=load(paste('/home/jc246980/DrySeason/Futuredat/Data/',es,"_",voi,sep='')) 		
		tdata=get(tdata)	
		tdata=cbind(pos,tdata)
	    tdata=merge(Ramsar_area_agg,tdata, by="UID", all.y=TRUE)			
	
		for(ram in RAMSARS) {
			
			temp = tdata[which(tdata$ramsar==ram),] 
			
			for(yy in YEARs) {
				
				temp2=temp[,grep(yy,colnames(temp))]
				outquant = t(apply(temp2,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out2 = (apply(outquant,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out = rbind(out, data.frame(ESs=es,RAMSARS=ram, YEARs=yy, Q10th=out2[2],Q50th=out2[5],Q90th=out2[8]))		
			
			}	
		}
	}
	write.csv(out,paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", gsub(".Rdata", "",voi),".csv", sep = ''), row.names = F )
}
	

vois_files=list.files("/home/jc246980/DrySeason/Currentdat/")

	for (voi in vois_files) {
		out=NULL
		tdata=load(paste('/home/jc246980/DrySeason/Currentdat/',voi,sep=''))
		tdata=get(tdata)
		tdata=cbind(pos,tdata)
	    tdata=merge(Ramsar_area_agg,tdata, by="UID", all.y=TRUE)			
		
		for(ram in RAMSARS) {

			temp = tdata[which(tdata$ramsar==ram),]
			temp_mean=as.data.frame(temp[,40])
			temp_sd=as.data.frame(temp[,41])
			outquant_mean = t(apply(temp_mean,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
			outquant_sd = t(apply(temp_sd,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
			out = rbind(out, data.frame(RAMSARS=ram,mean=outquant_mean [2],sd=outquant_sd[2])) 

		}

	write.csv(out,paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", voi,".csv", sep = ''), row.names = F )
	}

###################################################################################################
###Script to map shifts in climate space with respect to changes in dry season


		vois_files=c("num_month","total_severity","max_clust_length", "fut_clust_severity","fut_month_max_clust")		
		future_severity=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", vois_files[2],".csv", sep = ''))
		future_num_mth=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Future_", vois_files[1],".csv", sep = ''))
		
		vois_files=list.files("/home/jc246980/DrySeason/Currentdat/")
		current_severity=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", vois_files[5],".csv", sep = ''))
		current_num_mth=read.csv(paste("/home/jc246980/RAMSAR/Output/RAMSAR_dryseason_data/","Current_", vois_files[4],".csv", sep = ''))		
		
		yy=2085
		future_date_severity=future_severity[which((future_severity$YEARs==yy) & (future_severity$ESs==es)),]
		future_date_num_mth=future_num_mth[which((future_num_mth$YEARs==yy) & (future_num_mth$ESs==es)),]
		
		X_ellipse=as.data.frame((future_date_severity[,4]+future_date_severity[,6])/2)
		Y_ellipse=as.data.frame((future_date_num_mth[,4]+future_date_num_mth[,6])/2)		
		X_radius= abs((future_date_severity[,6]-X_ellipse[,1]))
		Y_radius=abs((future_date_num_mth[,6]-Y_ellipse[,1]))
		
		future_date=cbind(X_ellipse,Y_ellipse,X_radius,Y_radius)
		colnames(future_date) =c("X_ellipse","Y_ellipse", "X_radius", "Y_radius") 		
			
		png(paste(image.dir,es,"_",yy,"SeverityVSNoMth.png",sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=50, bg='white') 
		par(mfrow=c(1,1),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
		
			ylim=c(round(min(future_date_num_mth[,4:6])-0.5),round(max(future_date_num_mth[,4:6])+0.5))
			xlim=c(round(min(future_date_severity[,4:6])-50),round(max(future_date_severity[,4:6])+50))
			
			plot(rbind(future_date_severity[,5], current_severity[,2]) ,rbind(future_date_num_mth[,5], current_num_mth[,2]) ,xlim=xlim, ylim=ylim,xlab='Dry Season Severity', ylab='Number of months', 
			font.sub=2, font.lab=1,col = greycol(100), cex.lab=1.2, cex.axis=1, axes=T,xaxs='i',yaxs='i', col.axis='black', pch=20)	
			
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			
			
			for(i in 1:nrow(future_date)){
				filledellipse(future_date$X_radius[i], ry1 = future_date$Y_radius[i], mid = c(future_date$X_ellipse[i],future_date$Y_ellipse[i]),col="#698B2255",lty=3, lwd=2)
			}
			
			for(i in 1:nrow(current_severity)){
				filledellipse(current_severity$sd[i], ry1 = current_num_mth$sd[i], mid = c(current_severity$mean[i],current_num_mth$mean[i]),col="#66666622",lty=3, lwd=2)
			}
			

			for(i in 1:nrow(current_severity)){
			arrows(current_severity[,2], current_num_mth[,2], future_date_severity[,5], future_date_num_mth[,5], length = 0.25, angle = 30, lwd=4)
			}

		dev.off()





	

			
			
			
			
	
	
	
	
	