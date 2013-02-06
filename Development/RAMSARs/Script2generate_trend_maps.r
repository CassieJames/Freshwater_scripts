#Script to generate trend maps for each RAMSAR
# qsub -l nodes=2 -l pmem=5gb -I
# Code to produce trend graphs for ramsars
#required to run ... module load R-2.15.1
library(SDMTools);library(maptools); library(plotrix)
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
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/"
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/RAMSAR_trend_figures/"

### set up data frame to create names and zoom extent for each ramsar
RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load ramsar  asc
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
### Script to extract absolute annual values for figures
### Mean annual temperature

	
		# out=NULL
		# voi=c("tmp")
		# load(paste('/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/',voi,'.Rdata',sep='')) 		
		# annualdata=cbind(pos,annualdata)
		# annualdata=merge(Ramsar_area_agg,annualdata, by="UID", all.y=TRUE)			
		
		# for(ram in RAMSARS) {
			
			
			# tdata = annualdata[which(annualdata$ramsar==ram),] 
			
			
			# for(es in ESs) {
				
				# temp=tdata[,grep(es,colnames(tdata))]
				
				# for(year in YEARs) {
				
					# ttemp=temp[,grep(year,colnames(temp))]
					# outquant = t(apply(ttemp,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 	
					# out2 = (apply(outquant,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
					# out = rbind(out, data.frame(RAMSARS=ram,ESs=es, YEARs=year, Q10th=out2[2],Q50th=out2[5],Q90th=out2[8]))		
				# }	
			# }
		# }

	#write.csv(out,paste(image.dir,"Tmp_trendgraph_data.csv",sep=''),row.names=T)	
	
	out=read.csv(paste(image.dir,"Tmp_trendgraph_data.csv",sep='')) 
	

	for (ram in RAMSARS) {		
			

			Ramsar_name=ref_table[which(ref_table$ram==ram), 'RAMSAR_NAM'] [1]
			
			png(paste(image.dir,Ramsar_name,'_tmp_trends.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1], units='px', pointsize=20, bg='white') 
			par(mfrow=c(1,2),mar=c(5,5,2,1), oma=c(2,0,1,0)) 	
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85") |(out$RAMSARS==ram) &(out$ESs=="RCP45") ,]	
			graph_data=graph_data[,2:7]
			ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
	
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP45"),]	
			graph_data=graph_data[,2:7]
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Mean Annual Temperature', font.sub=2, font.lab=1, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.8, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey20')
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			abline(h=c(seq(ylim[1],ylim[2],1)),v=YEARs, col="white")
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs[2:7],labels=YEARs[2:7],lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	
			axis(2,seq(ylim[1],ylim[2],1),labels=round(seq(ylim[1],ylim[2],1)),lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	 	
			legend(2016,(((ylim[2]-(ylim[1]))/5)*4)+(ylim[1]), 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0, cex=1.8) 	
			legend(2018,(((ylim[2]-(ylim[1]))/5)*4.5)+(ylim[1]), 'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n', cex=1.8) 	
			mtext('Low (RCP45)', line=3,  side=1, cex=2,font=2)  	
		
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85"),]	
			graph_data=graph_data[,2:7]
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Mean Annual Temperature', font.sub=2, font.lab=1, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.8, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey20')
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			abline(h=c(seq(ylim[1],ylim[2],1)),v=YEARs, col="white")			
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs[2:7],labels=YEARs[2:7],lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	
			axis(2,seq(ylim[1],ylim[2],1),labels=round(seq(ylim[1],ylim[2],1)),lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	 	
			legend(2016,(((ylim[2]-(ylim[1]))/5)*4)+(ylim[1]), 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0, cex=1.8) 	
			legend(2018,(((ylim[2]-(ylim[1]))/5)*4.5)+(ylim[1]),'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n', cex=1.8) 	
			mtext('High (RCP85)', line=3,  side=1, cex=2,font=2)  
			
			dev.off() 
	}


### Precipitation

	
		# out=NULL
		# voi=c("pre")
		# load(paste('/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/',voi,'.Rdata',sep='')) 		
		# annualdata=cbind(pos,annualdata)
		# annualdata=merge(Ramsar_area_agg,annualdata, by="UID", all.y=TRUE)			
		
		# for(ram in RAMSARS) {
			
			
			# tdata = annualdata[which(annualdata$ramsar==ram),] 
			
			
			# for(es in ESs) {
				
				# temp=tdata[,grep(es,colnames(tdata))]
				
				# for(year in YEARs) {
				
					# ttemp=temp[,grep(year,colnames(temp))]
					# outquant = t(apply(ttemp,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 	
					# out2 = (apply(outquant,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
					# out = rbind(out, data.frame(RAMSARS=ram,ESs=es, YEARs=year, Q10th=out2[2],Q50th=out2[5],Q90th=out2[8]))		
				# }	
			# }
		# }

	# write.csv(out,paste(image.dir,"Pre_trendgraph_data.csv",sep=''),row.names=T)	
	
	out=read.csv(paste(image.dir,"Pre_trendgraph_data.csv",sep='')) 

	for (ram in RAMSARS) {		
			

			Ramsar_name=ref_table[which(ref_table$ram==ram), 'RAMSAR_NAM'] [1]
			
			png(paste(image.dir,Ramsar_name,'_pre_trends.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1], units='px', pointsize=20, bg='white') 
			par(mfrow=c(1,2),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85") |(out$RAMSARS==ram) &(out$ESs=="RCP45") ,]	
			graph_data=graph_data[,2:7]
			ylim=c(50*trunc(min(graph_data[,4:6])/50),50*ceiling(max(graph_data[,4:6])/50))
			

			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP45"),]	
			graph_data=graph_data[,2:7]
			#ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Annual Precipitation', font.sub=2, font.lab=1, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.8, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey90', col="grey20")
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			abline(h=c(seq(ylim[1],ylim[2],50)),v=YEARs, col="white")
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs[2:7],labels=YEARs[2:7],lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	
			axis(2,c(seq(ylim[1],ylim[2],50), ylim[2]),labels=round(c(seq(ylim[1],ylim[2],50), ylim[2])),lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	 	
			legend(2016,(((ylim[2]-(ylim[1]))/5)*1)+(ylim[1]), 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0, cex=1.8) 	
			legend(2018,(((ylim[2]-(ylim[1]))/5)*1.5)+(ylim[1]), 'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n', cex=1.8) 	
			mtext('Low (RCP45)', line=3,  side=1, cex=2,font=2)  	
		
			
			graph_data = out[(out$RAMSARS==ram) & (out$ESs=="RCP85"),]	
			graph_data=graph_data[,2:7]
			#ylim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(graph_data[,3],graph_data[,5],xlab='', ylab='Annual Precipitation', font.sub=2, font.lab=1, xlim=c(2015,2085),ylim=ylim, type='n', cex.lab=1.8, cex.axis=1, axes=F,xaxs='i',yaxs='i', col.axis='grey20')
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			abline(h=c(seq(ylim[1],ylim[2],50)),v=YEARs, col="white")
			polygon(c(graph_data[,3], rev(graph_data[,3])), c(graph_data[,4], rev(graph_data[,6])), col=adjustcolor('orange',alpha.f=0.5),lty=0) 	
			lines(graph_data[,3],graph_data[,5], col='grey20')  	
			axis(1,YEARs[2:7],labels=YEARs[2:7],lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	
			axis(2,c(seq(ylim[1],ylim[2],50), ylim[2]),labels=round(c(seq(ylim[1],ylim[2],50), ylim[2])),lwd=0.5,lwd.ticks=0.5,cex.axis=1.6,col='grey20') 	 	
			legend(2016,(((ylim[2]-(ylim[1]))/5)*1)+(ylim[1]), 'Best estimate (50th percentile)',lwd=1, bty='n',xjust=0, cex=1.8) 	
			legend(2018,(((ylim[2]-(ylim[1]))/5)*1.5)+(ylim[1]),'Variation between GCMs (10th-90th)', fill=adjustcolor('orange',alpha.f=0.5),border=adjustcolor('orange',alpha.f=0.5),bty='n', cex=1.8) 	
			mtext('High (RCP85)', line=3,  side=1, cex=2,font=2)  
			
			dev.off() 
	}






































