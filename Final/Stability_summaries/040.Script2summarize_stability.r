######################################################################W############################
#Script to determine stabilities for key river basins across Australia
#C. James..........................................14th January 2013

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
data.dir="/home/jc246980/Stability/Output/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085, 10)


####  summarise by drainage divisions
out.dir="/home/jc246980/Stability/River_basin_summaries/Outputs"
vois=list.files(data.dir)

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin  = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc)    
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

	for (voi in vois) { cat(voi,'\n') 
	
		outdelta=read.csv(paste(data.dir,voi,sep=''))
	
		table_delta = matrix(NA,nrow=length(RiverBasins)*3,ncol=(3*length(ESs)*length(YEARs))); #define the output matrix
		tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(table_delta) = tt 
		tt = expand.grid(c('quant_10', 'quant_50', 'quant_90'),RiverBasins); tt = paste(tt[,2],tt[,1],sep='_'); rownames(table_delta)=tt
	
		for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
				
				outdelta_rb = outdelta[which(outdelta$Riverbasin==rb),] #get the data only for the rb of interest
				
				outquant = apply(outdelta_rb[,c(8:103)],2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })#get the percentiles					
				rowname=paste(rb,"_quant_", c(10,50,90), sep='')
				table_delta[rownames(table_delta)==rowname,]=outquant[,]

		}; cat('\n')
			
	write.csv(table_delta,paste(out.dir,"rb_",voi,sep=''),row.names=T)	 
	
	}			

####  summarise by Ramsar
 
out.dir='/home/jc246980/Stability/Ramsars/Outputs/'

pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244   

wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_5km.Rdata',sep=''))
RAMSARS=unique(Ramsar_area_agg$ramsar)



	for (voi in vois) {
			
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

	}			
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 