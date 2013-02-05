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

image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/Climate_space/"


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
####Determine futures
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/"	
vois=c("tmp", "pre")

for(voi in vois) {
	out=NULL
	load(paste('/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/',voi,'.Rdata',sep='')) 		
	annualdata=cbind(pos,annualdata)
	annualdata=merge(Ramsar_area_agg,annualdata, by="UID", all.y=TRUE)			
	
	for(ram in RAMSARS) {
		
		tdata = annualdata[which(annualdata$ramsar==ram),] 
	
		for(es in ESs) {
			
			temp=tdata[,grep(es,colnames(tdata))]
			
			for(year in YEARs) {
			
				ttemp=temp[,grep(year,colnames(temp))]
				outquant = t(apply(ttemp,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out2 = (apply(outquant,2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) 
				out = rbind(out, data.frame(RAMSARS=ram,ESs=es, YEARs=year, Q10th=out2[2],Q50th=out2[5],Q90th=out2[8]))		
			}	
		}
	}
	
if (voi=="tmp") {out_tmp=out}
if (voi=="pre") {out_pre=out}	
write.csv(out,paste(image.dir,voi,"_climatespace_data.csv",sep=''),row.names=T)	

}	

###################################################################################################
####Determine currents



	###Code to determine mid point to draw ellipse and radius from that midpoint
	
	#rem to run again using pre now
	out_final=cbind(out_tmp,out_pre)
	
	out_final$X_ellipse=(out_final[,4]+out_final[,6])/2
	out_final$Y_ellipse=(out_final[,10]+out_final[,12])/2
	out_final$X_radius=(out_final$X_ellipse-out_final[,4])
	out_final$Y_radius=(out_final$Y_ellipse-out_final[,10])
	
	
	png(paste(image.dir,'climate_space_tmpVSpre_RCP85.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=50, bg='white') 
			par(mfrow=c(1,1),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
			graph_data = out_final[(out_final$ESs=="RCP85"),]
			graph_data_2015 = out_final[(out_final$ESs=="RCP85") & (out_final$YEARs==2015),]	
			graph_data_2085 = out_final[(out_final$ESs=="RCP85") & (out_final$YEARs==2085),]	
			ylim=c(round(min(graph_data[,10:12])-50),round(max(graph_data[,10:12])+50))
			xlim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(rbind(graph_data_2015[,5],graph_data_2085[,5]) ,rbind(graph_data_2015[,11],graph_data_2085[,11]) ,xlim=xlim, ylim=ylim,ylab='Annual Precipitation', xlab='Mean Annual Temperature', font.sub=2, font.lab=1,col = greycol(100), cex.lab=1.2, cex.axis=1, axes=T,xaxs='i',yaxs='i', col.axis='black', pch=20)			
			rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
			for(i in 1:nrow(graph_data_2085)){
				filledellipse(graph_data_2085[i,15], ry1 = graph_data_2085[i,16], mid = c(graph_data_2085[i,13],graph_data_2085[i,14]),col="#698B2255",lty=3, lwd=2)
			}


			for(i in 1:nrow(graph_data_2015)){
				filledellipse(graph_data_2015[i,15], ry1 = graph_data_2015[i,16], mid = c(graph_data_2015[i,13],graph_data_2015[i,14]), col = "gray20", lty=3)
			}		
	

			
			points(rbind(graph_data_2015[,5],graph_data_2085[,5]) ,rbind(graph_data_2015[,11],graph_data_2085[,11]) ,xlim=xlim, col="darkgrey", ylim=ylim,pch=10)		
			for(i in 1:nrow(graph_data_2015)){
					arrows(graph_data_2015[,5], graph_data_2015[,11], graph_data_2085[,5], graph_data_2085[,11], length = 0.25, angle = 30, lwd=4)
			}
	
	dev.off() 
			
			
			
			
	
	
	
	
	