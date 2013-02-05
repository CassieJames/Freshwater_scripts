######################################################################W############################
#Script to generate quantile maps for all RAMSARS
#C. James..........................................23rd Jan 2013

module load R-2.15.1

### start r
###Load necessary libraries
library(SDMTools); library(maptools); library(plotrix) #define the libraries needed
source('/home/jc148322/scripts/libraries/cool_functions.r')

###Set up directories

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
data.dir="/home/jc246980/Stability/Output/"
image.dir='/home/jc246980/RAMSAR/Output/RAMSAR_images/'

###Set up base files

base.asc = read.asc.gz('base.asc.gz')
baseasc=base.asc
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244   

###Load RAMSAR data

RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')

wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_5km.Rdata',sep=''))
RAMSARS=unique(Ramsar_area_agg$ramsar) 
 
#create the image of RAMasc to check whats going on
# cols = colorRampPalette(c('gray30','gray80','red','yellow','forestgreen'))(101)
# png(paste(image.dir,'RAMasc.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
	# image(RAMasc,ann=FALSE,axes=FALSE,col='BLACK')
# dev.off() 
 
 
# map annual percentile, SD and delta's for data annual mean temperature 

Ramsarshape = readShapePoly('/home/jc246980/RAMSAR/GISlayers/ramsar_wetlands_for_download.shp') #read in your shapefile
load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp

deltalims = c(0,6)                                                                         #define the delta limits
deltalabs = c(paste('<',deltalims[1]),3,paste('>',deltalims[2]))
sdlims = c(0,15) 
sdlabs = c(paste('<',sdlims[1]),7,paste('>',sdlims[2]))
cols = all.cols[21:1]

### set up data frame to create names and zoom extent for each ramsar
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

 
vois=list.files(data.dir, pattern="sd.csv")

wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_5km.Rdata',sep=''))
RAMSARS=unique(Ramsar_area_agg$ramsar)
#NB Kakadu is ram=RAMSARS[30]; Muir is ram=RAMSARS[23]
 
 for voi in vois) { 
		 
		 if (voi=="pre_sd.csv") {
		 cols=all.cols[1:21]
		 variable_name="Annual Precipitation"
		 }
		 
		 if (voi=="tmp_sd.csv") {
		 variable_name="Mean Annual Temperature"
		 }
		 
		if (voi=="tmn_sd.csv") {
		 variable_name="Minimum Temperature"
		 }
		 
		if (voi=="tmx_sd.csv") {
		 variable_name="Maximum Temperature"
		 }
		 
		 
		 if (voi=="total_severity_sd.csv") {
		 variable_name="Total Severity"
		 }
		
		 if (voi=="num_month_sd.csv") {
		 variable_name="Total Length"
		 }		
		
		 if (voi=="fut_clust_severity_sd.csv") {
		 variable_name="Cluster Severity"
		 }			
		
		 if (voi=="max_clust_length_sd.csv") {
		 variable_name="Cluster Length"
		 }					
		
		 if (voi=="fut_month_max_clust_sd.csv") {
		 variable_name="Cluster start"
		 }			
		 
		 yy="2085"
		 es="RCP85"
		 voi_name=gsub("_sd.csv","",voi)
		 
		tdata=read.csv(paste(data.dir, voi, sep=''))
		tdata=cbind(pos, tdata[,-c(1:7)])
		tdata=merge(Ramsar_area_agg,tdata, by="UID", all.y=TRUE)			
		dataoi_sd = tdata[,c(1:9,intersect(grep(yy,colnames(tdata)),grep(es,colnames(tdata))))]
		 
		tdata_delta=read.csv(paste(data.dir,voi_name,"_delta.csv", sep=''))
		tdata_delta=cbind(pos, tdata_delta[,-c(1:7)])
		tdata_delta=merge(Ramsar_area_agg,tdata_delta, by="UID", all.y=TRUE)			
		dataoi_delta = tdata_delta[,c(1:9,intersect(grep(yy,colnames(tdata_delta)),grep(es,colnames(tdata_delta))))]
		 
		for(ram in RAMSARS) {
				
				ram_data_sd=dataoi_sd[which(dataoi_sd$ramsar==ram),]
				ram_data_delta=dataoi_delta[which(dataoi_delta$ramsar==ram),]
				
				Ramsar_name=ref_table[which(ref_table$ram==ram), 'RAMSAR_NAM'] [1]
				Ramsar_zoom=ref_table[which(ref_table$ram==ram), 'zoom'] [1]
								
				deltalim_x=round(range(dataoi_delta[,10:12])[1],1)
				deltalim_y=round(range(dataoi_delta[,10:12])[2],1)
				deltalims=c(deltalim_x,deltalim_y)
				deltamid=round(((deltalims[1]+deltalims[2])/2),1)
				deltalabs = c(paste('<',deltalims[1]),deltamid,paste('>',deltalims[2]))
				
				sdlim_x=round(range(dataoi_sd[,10:12])[1],1)
				sdlim_y=round(range(dataoi_sd[,10:12])[2],1)
				sdlims=c(sdlim_x,sdlim_y)
				sdmid=round(((sdlims[1]+sdlims[2])/2),1)
				sdlabs = c(paste('<',sdlims[1]),sdmid,paste('>',sdlims[2]))
				
				assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(ram_data_sd$lon,ram_data_sd$lat, padding.percent=Ramsar_zoom)
				## this creates the mini-australia, called ‘clip’
				if (max.lat>=-18 & min.lat<=-34 |
								max.lon>=148 & min.lon<=120 ) {
								xlim=c(min(pos$lon),max(pos$lon));
								ylim=c(min(pos$lat),max(pos$lat))
								
				}else{
								xlim=c(min.lon,max.lon);
								ylim=c(min.lat,max.lat)
					 }

				setwd(image.dir)
			
				dir.create(paste(image.dir,voi_name,'/',sep=''),recursive=TRUE)
				
				png(paste(voi_name,"/",Ramsar_name,'.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+120, units='px', pointsize=20, bg='grey90')
			
				par(mar=c(2,2,2,2),cex=1,oma=c(6,2,2,2))

									  mat = matrix(c( 7,7,7,8,8,8,
													  7,7,7,8,8,8,
													  7,7,7,8,8,8,
													  1,1,1,4,4,4,
													  1,1,1,4,4,4,
													  1,1,1,4,4,4,
													  2,2,2,5,5,5,
													  2,2,2,5,5,5,
													  2,2,2,5,5,5,
													  3,3,3,6,6,6,
													  3,3,3,6,6,6,
													  3,3,3,6,6,6,
													  9,9,9,10,10,10,
													  9,9,9,10,10,10),nr=14,nc=6,byrow=TRUE) #create a layout matrix for images
			
			
			
				  layout(mat) #call layout as defined above
			
				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)  
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = dataoi_delta[,10]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols, add=TRUE) 
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey", lwd=1.5)		
				  mtext('10th', line=1,  side=2, cex=2.5)
				  
				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = dataoi_delta[,11]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols, add=TRUE) 
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)		  
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
				  mtext('50th', line=1,  side=2, cex=2.5)
				  
				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)	  
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = dataoi_delta[,12]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols, add=TRUE) 
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
				  mtext('90th', line=1,  side=2, cex=2.5)

				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] =  dataoi_sd[,10]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols, add=TRUE)
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)
				  
				  
				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] =  dataoi_sd[,11]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols, add=TRUE)
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
				  
				  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
				  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
				  tasc = baseasc; tasc[cbind(pos$row,pos$col)] =  dataoi_sd[,12]
				  image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols, add=TRUE)
				  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
				  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
				  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
			
				  assign.list(l,r,b,t) %=% par("usr") # make sure this follows a plot to get the extent correct!
				  image(base.asc,ann=FALSE,axes=FALSE,col='white', zlim=c(0,1))
				  image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="black",add=TRUE)
				  
				  plot(1:20,axes=FALSE,ann=FALSE,type='n')
				  text(10,15,Ramsar_name,cex=3) 
				  text(10,10,variable_name,cex=3) 
				  text(10,5,'RCP8.5 2085',cex=3) 

				  plot(1:20,axes=FALSE,ann=FALSE,type='n')
				  text(10,14,"Difference from current",cex=3)
				  color.legend(4,2,16,8,deltalabs,cols,align="rb",gradient="x", cex=2)
				  
				  plot(1:20,axes=FALSE,ann=FALSE,type='n')
				  text(10,14,"sd from current",cex=3)
				  color.legend(4,2,16,8,sdlabs,cols,align="rb",gradient="x", cex=2)
				  
				  dev.off() #close out the image
			
			
			}	
	
		}
	
	


	
	
	
	
	
	
	
	
	
  
  
  
  
  
  
  
  



  