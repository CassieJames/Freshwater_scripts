###################################################################################################
#get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }

library(SDMTools)
###Set up inputs
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)# read in positions on grid at 5 km resolution
pos$UID = 1:286244
load("/home/jc246980/Hydrology.trials/Aggregate_reach/Area_aggregated_by_UID_5km.Rdata") 


####Create column headings
YEARs=seq(2015, 2085,10)
mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
tt = expand.grid(mm,YEARs=YEARs);tt = paste(tt[,2],tt[,1],sep='_')
tt=c('SegmentNo',tt)

###Aggregate
load(paste(data.dir,"/",es,"_",gcm,".Rdata",sep='')) #loads Qrun
cois=colnames(Qrun) #all runoff columns to be manipulated later
out=cbind(pos,Qrun); rm(Qrun)
out=merge(Area_agg,out, by='UID')			
out[,cois] = (out$AREA/1000000) * out[,cois]  # Multiply area(in km) by runoff (in mm/km)
Runoff = aggregate(out[,cois], by = list(out$SegmentNo), sum)
colnames(Runoff) = tt #add the column names
save(Runoff, file=paste(out.dir,es,"_",gcm,".Rdata",sep='')) #save the runoff out	

