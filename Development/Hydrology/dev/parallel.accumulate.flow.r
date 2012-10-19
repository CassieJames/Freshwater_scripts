qsub -l nodes=2 -l pmem=5gb -I
# module load R-2.15.1
library(SDMTools)
library(parallel)

flowdata=readLines('/scratch/jc155857/graph_code/out_final_processed.csv') #read in the list of flow relationships
action=read.csv('/home/jc148322/NARPfreshwater/final_action.csv') #read in runoff

accflow=matrix(NA, nr=length(flowdata),nc=2) #make an empty matrix to put data into

tdata=strsplit(flowdata, ',') #strinsplit the flowdata on commas

accflow[,1] = as.vector(sapply(tdata,'[',1)) #print the output of first HydroIDs to another table

## -----------------------------------------------------------------------------------------
## best method so far:

tdata=lapply(tdata,as.numeric) #change flowdata from character to numeric

subtdata=tdata[1:10000]
subacc=accflow[1:10000,]

ncore = 10 #define the number of cores in teh cluster
cl <- makeCluster(getOption("cl.cores", ncore)) #define a cluster with the correct number of cores 
clusterExport(cl,'action') #export necessary objects to cores in cluster

accflow[,2]=parSapply(cl, tdata, function(ids) { return(sum(action$local_runoff[which(action$HydroID %in% ids)]*action$bi_prop[which(action$HydroID %in% ids)])) }) 
stopCluster(cl)

write.csv(accflow,'/home/jc148322/NARPfreshwater/acc_flow.csv',row.names=F)
# subacc[,2]=parSapply(cl, subtdata, function(ids) { return(sum(runoff$runoff[which(runoff$node_id %in% ids)])) }) 
# stopCluster(cl)




## -----------
#testing things

ncore = 10 #define the number of cores in teh cluster
cl <- makeCluster(getOption("cl.cores", ncore)) #define a cluster with the correct number of cores 


parSapply(cl, 1:15, get("+"), 2)
do.call("rbind", clusterCall(cl, function(cl) Sys.info()["nodename"]))