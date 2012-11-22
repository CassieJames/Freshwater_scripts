###################################################################################################
###Script to map distance to stability for RAMSARs
### C James...............20th November 2012

#qsub -l nodes=2 -l pmem=5gb -I
#required to run ... module load R-2.15.1

###Load necessary libraries
library(SDMTools); library(maptools); library(plotrix) 
source('/home/jc148322/scripts/libraries/cool_functions.r')

###Set up directories

wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
data.dir="/home/jc246980/RAMSAR/Output/RAMSAR_site_data/tmp/"
out.dir='/home/jc246980/RAMSAR/Output/RAMSAR_quantile_data/'
image.dir="/home/jc246980/RAMSAR/Output/RAMSAR_images/"

###Set up base files

base.asc = read.asc.gz('base.asc.gz');
baseasc=base.asc
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
ESs=list.files(future.dir, pattern='RCP')
YEARs = seq(2015, 2085,10)     

###Set ESs and time period 

tdata=load('/home/jc165798/working/NARP_hydro/stability/OZ_5km/distance_stability/tmp_RCP45_2085.Rdata') 	
outquant = t(apply(outdist,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles


###Load RAMSAR data

RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load ramsar  asc
RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')
pos$RAMSAR  = extract.data(cbind(pos$lon,pos$lat), RAMasc)    
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector

Ramsarshape = readShapePoly('/home/jc246980/RAMSAR/GISlayers/ramsar_wetlands_for_download.shp') #read in your shapefile
load('/home/jc246980/RAMSAR/RCP85_percentiles.Rdata') 	#load the annual temperature percentiles for 2085 as an example 
load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp

lims10 = c(min(outquant[,1], na.rm = TRUE),max(outquant[,1], na.rm = TRUE))      
lims10lab=c(paste('<',lims10[1]),paste('>',lims10[2]))
lims50 = c(min(outquant[,2], na.rm = TRUE),max(outquant[,2], na.rm = TRUE))   
lims50lab=c(paste('<',lims50[1]),paste('>',lims50[2]))
lims90 = c(min(outquant[,3], na.rm = TRUE),max(outquant[,3], na.rm = TRUE))                                                                      #define the delta limits
lims90lab=c(paste('<',lims90[1]),paste('>',lims90[2]))


cols = all.cols[21:1]

### set up data frame to create names and zoom extent for each ramsar
ram= c(13,27,26,32,31,33,9,15,17,14,12,6,1,3,2,5,24,4,29,22,28,36,37)
refcode=c(64,56,55,39,38,36,24,52,28,65,53,50,43,41,51,44,34,42,33,32,31,2,1)
zoom=c(20,100,150,150,100,50,200,15,15,15,200,20,50,20,20,20,20,20,50,30,30,30,30)
refcode_names=data.frame(ram,refcode,zoom)


for(ram in RAMSARS) {
		
		outdat=cbind(pos[,1:5],outquant)
		tdata = outdat[which(outdat$RAMSAR==ram),] #get the data only for the RAMSAR of interest

		assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(tdata$lon,tdata$lat, padding.percent=20)
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
		Ramsar_name=RAMinfo$RAMSAR_NAM[which(RAMinfo$REFCODE==64)][1]
		png(paste(Ramsar_name,'trial2.png',sep=''),width=dim(baseasc)[1]*3+120, height=dim(baseasc)[1], units='px', pointsize=20, bg='white')
	
		par(mar=c(2,2,2,2),cex=1,oma=c(6,2,2,2))

                              mat = matrix(c( 
											  1,1,1,2,2,2,3,3,3,
											  1,1,1,2,2,2,3,3,3,
											  1,1,1,2,2,2,3,3,3,
											  4,4,4,5,5,5,6,6,6),nr=4,nc=9,byrow=TRUE) #create a layout matrix for images
	
	
	
		  layout(mat) #call layout as defined above
	
		  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
		  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)  
		  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = outdat[,6]
		  image(tasc,ann=FALSE,axes=FALSE,zlim=lims10,col=cols, add=TRUE) 
		  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
          plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
		  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey", lwd=1.5)		
		  mtext('10th', line=1,  side=1, cex=2.5)
		  
		  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
		  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
		  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = outdat[,7]
		  image(tasc,ann=FALSE,axes=FALSE,zlim=lims50,col=cols, add=TRUE) 
		  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
          plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)		  
	      plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
		  mtext('50th', line=1,  side=1, cex=2.5)
		  
		  image(baseasc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
		  image(baseasc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)	  
		  tasc = baseasc; tasc[cbind(pos$row,pos$col)] = outdat[,8]
		  image(tasc,ann=FALSE,axes=FALSE,zlim=lims90,col=cols, add=TRUE) 
		  plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')
   		  plot(Ramsarshape, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
		  plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE,border="darkgrey",  lwd=1.5)	
		  mtext('90th', line=1,  side=1, cex=2.5)


		  plot(1:20,axes=FALSE,ann=FALSE,type='n')
		  # text(10,14,"Difference from current",cex=3)
		  color.legend(4,2,16,8,lims10lab,cols,align="rb",gradient="x")
		  
		  plot(1:20,axes=FALSE,ann=FALSE,type='n')
		  # text(10,14,"sd from current",cex=3)
		  color.legend(4,2,16,8,lims50lab,cols,align="rb",gradient="x")
		  
		  plot(1:20,axes=FALSE,ann=FALSE,type='n')
		  # text(10,14,"sd from current",cex=3)
		  color.legend(4,2,16,8,lims90lab,cols,align="rb",gradient="x")
		  		  
		  
		  
		  
		  dev.off() #close out the image
	
	
	}	



















