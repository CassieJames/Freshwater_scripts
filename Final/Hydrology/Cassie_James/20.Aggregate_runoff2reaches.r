################################################################################
# aggregate runoff generated at 5km resolution onto Janets reaches

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244 

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd)   

load("/home/jc246980/Hydrology.trials/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata")  # Load runoff
load("/home/jc246980/Hydrology.trials/Area_aggregated_by_UID_5km.Rdata") 


# create means for 30 years

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun) = tt #add the column names

Q_run_curmean_dynamo=NULL
Q_run_cursd_dynamo=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
           Q_run_cursd_dynamo=cbind(Q_run_cursd_dynamo,tdata_cursd)
      }


# calculate annual total runoff per km2 for now

pos$runoff_rate =  rowSums(Q_run_curmean_dynamo)         

FINAL<- merge(Area_agg, pos, by='UID')                                          # Merge Area_agg with 5km pos file                            

FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$runoff_rate                         # Multiply area(in km) by runoff (in mm/km)
 
Reach_runoff = aggregate(FINAL$Runoff, by = list(FINAL$REACHID), sum) 

colnames(Reach_runoff) =c('REACHID', 'Runoff')

save(Reach_runoff,file=paste(wd,'Reach_runoff_5km.Rdata',sep=''))     


