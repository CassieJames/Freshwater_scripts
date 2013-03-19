###################################################################################################
#### Summarise the current runoff

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

W##################################################################################################
#### create pos file at 1 km resolution and map accumulated flow to pos

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz') # load 1 km raster and create pos file
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]     
pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)    
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

current=read.csv("/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/Current_static.csv")
current$annualsum=rowSums(current[, 2:13])
pos=merge(pos,current,by='SegmentNo',all.x=TRUE)

out.dir= "/home/jc246980/Final report/Summary_data/"

table_delta = matrix(NA,nrow=length(RiverBasins),ncol=4); #define the output matrix
colnames(table_delta) = c('quant_10', 'quant_50', 'quant_90', 'maximum') 
rownames(table_delta)=RiverBasins
	
	for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
					
		out_rb = pos[which(pos$Riverbasin==rb),] #get the data only for the rb of interest
		outquant = t(apply(as.data.frame(out_rb[,"annualsum"]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))#get the percentiles					
		table_delta[rownames(table_delta)==rb,1:3]=outquant[,]
		table_delta[rownames(table_delta)==rb,'maximum']=max(na.omit(out_rb[,"annualsum"]))
		
	}; cat('\n')
			
write.csv(table_delta,paste(out.dir,"rb_runoff_accumulated_current.csv",sep=''),row.names=T)	 

###################################################################################################
### Create summaries for runoff

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz') # load 1 km raster and create pos file
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]      
pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)    
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

current=load("/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/Current_static.Rdata")
current=get(current)
current$annualsum=rowSums(current[, 2:13])
pos=merge(pos,current,by='SegmentNo',all.x=TRUE)

out.dir= "/home/jc246980/Final report/Summary_data/"	

table_delta = matrix(NA,nrow=length(RiverBasins),ncol=4); #define the output matrix
colnames(table_delta) = c('quant_10', 'quant_50', 'quant_90', 'maximum') 
rownames(table_delta)=RiverBasins
	
	for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
					
		out_rb = pos[which(pos$Riverbasin==rb),] #get the data only for the rb of interest
		outquant = t(apply(as.data.frame(out_rb[,"annualsum"]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))#get the percentiles					
		table_delta[rownames(table_delta)==rb,1:3]=outquant[,]
		table_delta[rownames(table_delta)==rb,'maximum']=max(na.omit(out_rb[,"annualsum"]))
		
	}; cat('\n')
			
write.csv(table_delta,paste(out.dir,"rb_runoff_current.csv",sep=''),row.names=T)	 
