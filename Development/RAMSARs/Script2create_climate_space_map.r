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
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/tmp/"
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/"

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


###Script to determine percentiles for absolute values
voi=c("pre")
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
	
	###Code to determine mid point to draw ellipse and radius from that midpoint
	out_final=out
	#rem to run again using pre now
	out_final=cbind(out_final,out)
	
	out_final$X_ellipse=(out_final[,4]+out_final[,6])/2
	out_final$Y_ellipse=(out_final[,10]+out_final[,12])/2
	out_final$X_radius=(out_final$X_ellipse-out_final[,4])
	out_final$Y_radius=(out_final$Y_ellipse-out_final[,10])
	
	
	png(paste(image.dir,'climate_space_tmxVSpre.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=50, bg='#F0F8FF') 
			par(mfrow=c(1,1),mar=c(5,5,2,1), oma=c(0,0,1,0)) 	
			graph_data = out_final[(out_final$ESs=="RCP85"),]
			graph_data_2015 = out_final[(out_final$ESs=="RCP85") & (out_final$YEARs==2015),]	
			graph_data_2085 = out_final[(out_final$ESs=="RCP85") & (out_final$YEARs==2085),]	
			ylim=c(round(min(graph_data[,10:12])-50),round(max(graph_data[,10:12])+50))
			xlim=c(round(min(graph_data[,4:6])-0.5),round(max(graph_data[,4:6])+0.5))
			plot(rbind(graph_data_2015[,5],graph_data_2085[,5]) ,rbind(graph_data_2015[,11],graph_data_2085[,11]) ,xlim=xlim, ylim=ylim,ylab='Annual Precipitation', xlab='Maximum Monthly Temperature', font.sub=2, font.lab=3,col = greycol(100), cex.lab=1.2, cex.axis=1, axes=T,xaxs='i',yaxs='i', col.axis='black', pch=20)			
	
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
			
			
			
			
	
	
	
	
	