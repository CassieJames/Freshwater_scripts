### make runoff csv
network=read.csv('/home/jc148322/NARPfreshwater/network.csv',as.is=T)

wd = "/home/jc246980/Hydrology.trials/"; setwd(wd) 
load(file=paste(wd,'Reach_runoff_5km.Rdata',sep=''))  
runoff=Reach_runoff
runoff=runoff[which(runoff$SegmentNo %in% network$SegmentNo),]
runoff=merge(network[,c('HydroID','SegmentNo')],runoff,by='SegmentNo')

###first steps of action csv - from Cassie

segLength=aggregate(runoff$SegLength, by = list(runoff$SegmentNo), sum)     
colnames(segLength)=c('SegmentNo','TotLength')

runoff=merge(runoff,segLength, by='SegmentNo')

runoff$SegProp=runoff$SegLength/runoff$TotLength
#############################################################################
##CASSIE'S SCRIPT GOES HERE
#############################################################################


####complete action csv
network=read.csv('/home/jc148322/NARPfreshwater/network.csv',as.is=T)
action=read.csv('/home/jc148322/NARPfreshwater/action.csv', as.is=T) #read in runoff


action=merge(action,network[,c('HydroID','From_Node','Area')],by='HydroID')
action$Hydro_Area=action$seg_prop*action$Area


totArea=aggregate(action$Hydro_Area, by = list(action$From_Node), sum) 
colnames(totArea)=c('From_Node','TotArea')


action=merge(action,totArea,by='From_Node')

action$bi_prop=action$Hydro_Area/action$TotArea

##checking
d=action$From_Node[duplicated(action$From_Node)]
tdata=action[which(action$From_Node %in% d),]
s=tdata$SegmentNo[duplicated(tdata$SegmentNo)]
segdata=tdata[which(tdata$SegmentNo %in% s),]

s=action$From_Node[duplicated(action$From_Node)]

##checking end

write.csv(action,'/home/jc148322/NARPfreshwater/action_step2.csv',row.names=F)

action=action[,c(1:6,10)]
action$local_runoff=action$runoff*action$seg_prop
write.csv(action,'/home/jc148322/NARPfreshwater/final_action.csv',row.names=F)

