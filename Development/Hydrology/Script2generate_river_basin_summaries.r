###################################################################################################
### Code to compare accumulated and absolute runoff for river basins across Australia

library(SDMTools) #define the libraries needed

##########set up current climate baseline and attribute identifier
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
pos$UID = 1:286244 																# append unique identifier

##########import river basin asc and attribute river basin number to position table

river.basin.dir='/home/jc246980/Hydrology.trials/Flow_accumulation/';  setwd(river.basin.dir)
tasc = read.asc("bname.asc")
pos$riverbasin  = extract.data(cbind(pos$lon,pos$lat), tasc)  
RB=unique(pos$riverbasin)

##########import weights based on proportion of reach id area in each segment

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Area_agg_weights.Rdata') #load relationships table with weights (Reach_area_agg)
Merged<- merge(Reach_area_agg, pos, by='UID')  


#########Import network attrbutes and extract hydroID and SegmentNo id table and merge with data

network.file="/home/jc165798/working/NARP_hydro/flow_accumulation/NetworkAttributes.csv" #define the name of the network attribute data file
network = read.csv(network.file,as.is=TRUE) #read in the network attribute data
networkids=network[,c(2,9)]
Merged2<- merge(networkids, Merged, by='SegmentNo') 

##########Load accumulated data and merge with above table and apply weihts to runoff values
#Need to work out how to get the catchment pour point

# acc.data=read.csv("monthly_flow_dynamic.csv")

# Merged3=merge(Merged2, acc.data, by='HydroID')

# Merged4=Merged3[,6]*Merged3[,c(12:23)]

# annual_sum = rowSums(Merged4)

# Merged5=cbind(Merged3,annual_sum)


##########Load raw runoff data (not accumulated!)

load("/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
Qrun_agg=Qrun

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun_agg) = tt #add the column names

Q_run_curmean_dynamo=NULL

    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

	   tdata_curmean = rowMeans(Qrun_agg[,which(as.numeric(substr(colnames(Qrun_agg),1,2))==mm)],na.rm=TRUE) #calculate row means
	   Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
    }

mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
tt = expand.grid(mm,"Cur_dyn");tt = paste(tt[,2],tt[,1],sep='_'); colnames(Q_run_curmean_dynamo) = tt #add the column names

Q_run_curmean_dynamo=as.data.frame(Q_run_curmean_dynamo) # Monthly data

tdata = cbind(pos, Q_run_curmean_dynamo)
tdata$annual=rowSums(tdata[,7:18])

runoff_sum = aggregate(tdata$annual, by = list(tdata$riverbasin), sum)  #aggregate annual runoff by river basin
wd="/home/jc246980/Hydrology.trials/"
write.csv(runoff_sum,paste(wd,'Runoff_summed_not_accumulated.csv',sep=''))

###################################################################################################

out=NULL
	for (river in RB) {
            
			tdata = Merged5[which(Merged5$riverbasin==river),] #get the data only for the river basin of interest		
			out2=max(tdata$annual_sum)
			out = rbind(out, data.frame(RB=river,max_accumulated_flow=out2[1]))

	}



















