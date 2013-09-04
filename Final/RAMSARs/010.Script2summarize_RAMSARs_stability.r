######################################################################W############################
#Script to determine stabilities for 31 RAMSARs across Australia
#C. James..........................................14th January 2013

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244       

future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
out.dir="/home/jc246980/Stability/Ramsars/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085, 10)

###Load river basin info

                                                     	# add unique identifier to each 5km grid cell
wd='/home/jc246980/RAMSAR/'                
load(paste(wd,'Area_aggregated_by_ramsar_5km.Rdata',sep=''))      
Ramsars = unique(na.omit(Ramsar_area_agg$ramsar)) # create river basin vector

###################################################################################################
####determine average stability for temperature mean, max, min, and precipitation

data.dir ="/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/annual/"
vois=c("tmp", "tmx", "tmn", "pre")

for (voi in vois) { cat(voi,'\n') #cycle through each variable of interest
	load(paste(data.dir,voi,'.Rdata',sep='')) #load the data
	annualdata=cbind(pos[,1:5],annualdata)
	annualdata=merge(Ramsar_area_agg, annualdata, by='UID') # merge annual data with Area agg file
	
	Mean_annualdata_rb = matrix(NA,nrow=length(Ramsars),ncol=ncol(annualdata)) #define temporary output matrix
	colnames(Mean_annualdata_rb)=colnames(annualdata); rownames(Mean_annualdata_rb)=Ramsars
	
	outdelta = matrix(NA,nrow=length(Ramsars),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
	tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt; rownames(outdelta)=Ramsars#add the column names
	outsd = outdelta #copy the outdelta
	

	for (ram in Ramsars) { cat(ram,'\n') #cycle through each basin
			
			
			annualdata_rb = annualdata[which(annualdata$ramsar==ram),] #get the data only for the ramsar of interest
			
		    for (ii in 10:ncol(annualdata_rb)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
				tdata = as.data.frame(annualdata_rb$weights *annualdata_rb[,ii]) 
				tdata=cbind(annualdata_rb[,c(1:9)], tdata) 
				colnames(tdata)[10]="Wt_data"
				tdata = aggregate(tdata$Wt_data, by = list(tdata$ramsar), sum) 
				Mean_annualdata_rb[rownames(Mean_annualdata_rb)==ram,2]=tdata[[1]]
				Mean_annualdata_rb[rownames(Mean_annualdata_rb)==ram,ii]=tdata[[2]]
				
			}; cat('\n')
	}		
			
			Mean_annualdelta_rb = Mean_annualsd_rb = Mean_annualdata_rb #copy annualdata to be replaced with number of standard deviation and percentiles		
			
			for (ii in 12:ncol(Mean_annualdata_rb)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
				Mean_annualsd_rb[,ii] = (Mean_annualdata_rb[,ii]-Mean_annualdata_rb[,10])/Mean_annualdata_rb[,11] #calculate the number of standard deviations away
			}; cat('\n')
			
			if (voi=='pre') { 
				Mean_annualdelta_rb[,-c(1:11)] = Mean_annualdelta_rb[,-c(1:11)]/Mean_annualdelta_rb[,10] #convert to deltas as a proportion
			}else{Mean_annualdelta_rb[,-c(1:11)] = Mean_annualdelta_rb[,-c(1:11)]-Mean_annualdelta_rb[,10]} #convert to deltas as an absolute difference
			
			
			###now create and save the percentiles
			
			for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse
				
				for (year in YEARs) { cat(year,'\n') #cycle through each of the years 
					
					cois = intersect(grep(year,colnames(Mean_annualdelta_rb)),grep(es,colnames(Mean_annualdelta_rb))) #define the columns that intersect the ES & year
					outquant = t(apply(as.data.frame(Mean_annualdelta_rb[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
					outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
					outquant = t(apply(as.data.frame(Mean_annualsd_rb[,cois]),1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
					outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data

				}
			}
	}
	
	write.csv(outdelta,paste(out.dir,voi,"_delta.csv",sep=''),row.names=T)	 
	write.csv(outsd,paste(out.dir,voi,"_sd.csv",sep=''),row.names=T)	 
}
 
 
 
###################################################################################################
### Determine average stability runoff delta summaries for river basins
 
data.dir = "/home/jc246980/Hydrology.trials/Stability/Runoff/Data/"
wd ="/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/TomHarwood_data/" # location of current runoff
load(paste(wd, "Qrun.current_5km_means.Rdata", sep=''))
Current=as.matrix(rowSums(Qrun))

outdelta = matrix(NA,nrow=length(RiverBasins),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt;rownames(outdelta)=RiverBasins #add the column names

	for (es in ESs) {
	
	load(paste(data.dir,es,'_data_runoff.Rdata',sep='')) #load the data
	tdata=cbind(pos[,1:5],Current)
	data.runoff=cbind(tdata,data.runoff)
 
	 for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
		
				data.runoff.rb = data.runoff[which(data.runoff$Riverbasin==rb),] #get the data only for the rb of interest
				mean.runoff.rb=as.data.frame(colMeans(data.runoff.rb)) #		
				mean.runoffdelta.rb =  mean.runoff.rb #copy annualdata to be replaced with number of standard deviation and percentiles		
				
				for (ii in 7:nrow(mean.runoff.rb)) { cat(ii,'...') #cycle through each of the gcm and year combinations
			
					mean.runoffdelta.rb[ii,] = mean.runoff.rb[ii,]/(mean.runoff.rb['Current',]+0.000001)#convert to deltas as a proportion
				}
				
				###now create and save the percentiles

					for (year in YEARs) { cat(year,'\n') #cycle through each of the years 
						
						rois = grep(year,rownames(mean.runoffdelta.rb)) #define the columns that intersect the ES & year
						outquant = t(apply(as.data.frame(mean.runoffdelta.rb[rois,]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
						outdelta[rownames(outdelta)==rb,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data

					}
				
	    }
	}

write.csv(outdelta,paste(out.dir,"/Runoff_delta.csv",sep=''),row.names=T)	

 

 
################################################################################################### 
### Determine average stability for dry season metrics for river basins
 
# load current data
cur.dir = "/home/jc246980/DrySeason/Currentdat"

load(paste(cur.dir, '/DrySeason_num.month.Rdata', sep=''))  # num.month
current_num_month=num.month[,c(31:32)]

load(paste(cur.dir, '/DrySeason_total.severity.Rdata', sep='')) # total.severity
current_total_severity=total.severity[,c(31:32)]

load(paste(cur.dir, '/DrySeason_max.clust.length.Rdata', sep='')) # max.clust.length
current_max_clust_length=max.clust.length[,c(31:32)]

load(paste(cur.dir, '/DrySeason_clust.severity.Rdata', sep=''))    # clust.severity
current_fut_clust_severity=clust.severity[,c(31:32)]

load(paste(cur.dir, '/DrySeason_month.max.clust.Rdata',sep=''))   #  month.max.clust
current_fut_month_max_clust=month.max.clust[,c(31:32)]

data.dir="/home/jc246980/DrySeason/Futuredat/Data/"
vois_data=c("num_month","total_severity","max_clust_length", "fut_clust_severity","fut_month_max_clust")

	for (voi in vois_data){ cat(voi,'\n')
		
		outdelta = matrix(NA,nrow=length(RiverBasins),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
		tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt; rownames(outdelta)=RiverBasins#add the column names
		outsd = outdelta #copy the outdelta
		
		current=paste("current_", voi, sep='')
		current=get(current)
		
		for (es in ESs){ cat(es,'\n')
				
				tdata=load(paste(data.dir,es,'_',voi,'.Rdata', sep=''))
				tdata=get(tdata)
				temp=cbind(pos[,1:5],current)
				tdata=cbind(temp,tdata)
				
				for (rb in RiverBasins) { cat(rb,'\n')
					
					data.dryseason.rb = tdata[which(tdata$Riverbasin==rb),] #get the data only for the rb of interest
					mean_dryseason_rb=as.data.frame(colMeans(data.dryseason.rb))
					mean_dryseasondelta_rb = mean_dryseasonsd_rb = mean_dryseason_rb #copy annualdata to be replaced with number of standard deviation and percentiles		
					
					for (ii in 8:nrow(mean_dryseason_rb)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
						mean_dryseasonsd_rb[ii,] = (mean_dryseason_rb[ii,]-mean_dryseason_rb[6,])/mean_dryseason_rb[7,] #calculate the number of standard deviations away
					}; cat('\n')
				
					if (voi=='total_severity'| voi=='fut_clust_severity') { 
						mean_dryseasondelta_rb[-c(1:7),] = mean_dryseasondelta_rb[-c(1:7),]/mean_dryseasondelta_rb[6,] #convert to deltas as a proportion
					}else{mean_dryseasondelta_rb[-c(1:7),] = mean_dryseasondelta_rb[-c(1:7),]-mean_dryseasondelta_rb[6,]} #convert to deltas as an absolute difference
				
						for (year in YEARs){   cat(year,'\n')
									 
							rois = grep(year,rownames(mean_dryseasondelta_rb)) # define the year
							outquant = t(apply(as.data.frame(mean_dryseasondelta_rb[rois,]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
							outdelta[rownames(outdelta)==rb,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
							outquant = t(apply(as.data.frame(mean_dryseasonsd_rb[rois,]),2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
							outsd[rownames(outsd)==rb,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data

					}
				}
		}
		
		write.csv(outdelta,paste(out.dir,voi,"_delta.csv",sep=''),row.names=T)	 
		write.csv(outsd,paste(out.dir,voi,"_sd.csv",sep=''),row.names=T)	 
	 
	 }
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 