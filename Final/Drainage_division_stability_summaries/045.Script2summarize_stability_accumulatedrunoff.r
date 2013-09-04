###################################################################################################
#### Summarise the stability of the accumulated runoff

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

#### create pos file at 1 km resolution and map accumulated flow to pos

raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz')
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]    
base.asc=raster   
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile			
data.dir="/home/jc246980/Stability/Output/"
out.dir= "/home/jc246980/Stability/River_basin_summaries/Output/"
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
ESs = list.files(futdir, pattern="RCP") 	
YEARs=seq(2015, 2085, 10)

outdelta=read.csv(paste(data.dir,"Accumulated_runoff_delta.csv",sep=''))
colnames(outdelta)[1]="SegmentNo"	
save(outdelta,file=paste(data.dir,"Accumulated_runoff_delta.Rdata",sep=''))

pos=merge(pos,outdelta,by='SegmentNo',all.x=TRUE)

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)    
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

table_delta = matrix(NA,nrow=length(RiverBasins)*3,ncol=(3*length(ESs)*length(YEARs))); #define the output matrix
tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(table_delta) = tt 
tt = expand.grid(c('quant_10', 'quant_50', 'quant_90'),RiverBasins); tt = paste(tt[,2],tt[,1],sep='_'); rownames(table_delta)=tt
	
	for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
					
		outdelta_rb = pos[which(pos$Riverbasin==rb),] #get the data only for the rb of interest
		
		outquant = apply(outdelta_rb[,c(7:102)],2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })#get the percentiles					
		rowname=paste(rb,"_quant_", c(10,50,90), sep='')
		table_delta[rownames(table_delta)==rowname,]=outquant[,]

	}; cat('\n')
			
write.csv(table_delta,paste(out.dir,"rb_runoff_accumulated_delta.csv",sep=''),row.names=T)	 



####  summarise by Ramsar
 
out.dir='/home/jc246980/Stability/Ramsars/Outputs/'

raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz')
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]    

wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_1km.Rdata',sep=''))
RAMSARS=unique(Ramsar_area_agg$ramsar)
voi="Accumulated_runoff_delta.csv"
			
	outdelta=read.csv(paste(data.dir,voi,sep=''))
	tdata=cbind(pos, outdelta[,-c(1:7)])
	tdata=merge(Ramsar_area_agg,tdata, by="UID", all.x=TRUE)																		
	
	table_delta = matrix(NA,nrow=length(RAMSARS)*3,ncol=(3*length(ESs)*length(YEARs))); #define the output matrix
	tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(table_delta) = tt 
	tt = expand.grid(c('quant_10', 'quant_50', 'quant_90'),RAMSARS); tt = paste(tt[,2],tt[,1],sep='_'); rownames(table_delta)=tt
			
	for (ram in RAMSARS) { cat(ram,'\n') #cycle through each basin
			
			outdelta_ram = tdata[which(tdata$ramsar==ram),] #get the data only for the rb of interest
		
			outquant = apply(outdelta_ram[,c(10:105)],2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }) #get the percentiles					
			rowname=paste(ram,"_quant_", c(10,50,90), sep='')
			table_delta[rownames(table_delta)==rowname,]=outquant[,]
		
	}; cat('\n')
				

	write.csv(table_delta,paste(out.dir,"ram_",voi,sep=''),row.names=T)	 

			
 
