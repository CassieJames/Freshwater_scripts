qsub -l nodes=2 -l pmem=5gb -I
# load R
library(SDMTools)

flowdata=readLines('/scratch/jc155857/graph_code/out_final_processed.csv') #read in the list of flow relationships
runoff=read.csv('/home/jc148322/NARPfreshwater/runoff.csv') #read in runoff

accflow=matrix(NA, nr=length(flowdata),nc=2) #make an empty matrix to put data into

tdata=strsplit(flowdata, ',') #strinsplit the flowdata on commas

accflow[,1] = as.vector(sapply(tdata,'[',1)) #print the output of first HydroIDs to another table

## -----------------------------------------------------------------------------------------
## best method so far:

tdata=lapply(tdata,as.numeric) #turn flo
accflow[,2]=sapply(tdata, function(ids) { return(sum(runoff$runoff[which(runoff$node_id %in% ids)])) }) 



## -----------------------------------------------------------------------------------------
#method 1
colnames(subacc)=c('HydroID','acc_runoff')
subacc=as.data.frame(subacc) #turn it into a dataframe so that we can subset with $
subacc[,2]=NA
i=0
system.time(
for (id in subacc$HydroID) { cat(id, '\n')
	i=i+1
	#sum runoff from runoff table for all listed in flowdata
	v=strsplit(subflow[[i]],',') # isolate the list associated with HydroID
	ids=as.vector(v[[1]]) #turn the list into a vector
	tt=sum(runoff$runoff[which(runoff$node_id %in% ids)]) # find the vector in node_id, and sum the runoff values for all
	
	
	subacc$acc_runoff[which(subacc$HydroID==id)]=tt #print accumulated runoff to another table
	
})


#way too slow
## -----------------------------------------------------------------------------------------
#method 2 - not any faster
subacc=accflow[1:10,]

sumflow= function(i) {
	v=strsplit(subflow[[i]],',') # isolate the list associated with HydroID
	ids=as.vector(v[[1]]) #turn the list into a vector
	tt=sum(runoff$runoff[which(runoff$node_id %in% ids)]) # find the vector in node_id, and sum the runoff values for all
	
return(tt)
}

system.time(
for (i in 1:nrow(subacc)) { cat(i,'\n')
	
	subacc[i,2]=sumflow(i) #print accumulated runoff to another table
	
})

## -----------------------------------------------------------------------------------------
#method 3
subacc=accflow[1:100,]
subtdata=tdata[1:100]

subtdata=lapply(subtdata,as.numeric)
subacc[,2]=sapply(subtdata, function(ids) { return(sum(runoff$runoff[which(runoff$node_id %in% ids)])) }) 

system.time(sapply(subtdata, function(ids) { return(sum(runoff$runoff[which(runoff$node_id %in% ids)])) }) )





