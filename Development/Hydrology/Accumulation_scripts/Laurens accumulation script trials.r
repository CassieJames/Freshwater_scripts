qsub -l nodes=2 -l pmem=5gb -I
# load R

library(SDMTools)

flowdata=readLines('/scratch/jc155857/graph_code/out_final_processed.csv') #read in the list of flow relationships
runoff=read.csv('/home/jc148322/NARPfreshwater/runoff.csv') #read in runoff
subflow=flowdata[1:100] #subset flow relationships list for practicing

accflow=matrix(NA, nr=length(subflow),nc=2) #make an empty matrix to put data into

tdata=strsplit(subflow, ',') #strinsplit the flowdata on commas

accflow[,1] = as.vector(sapply(tdata,'[',1)) #print the output of first HydroIDs to another table
colnames(accflow)=c('HydroID','acc_runoff')
accflow=as.data.frame(accflow) #turn it into a dataframe so that we can subset with $
accflow[,2]=NA

i=0
system.time(
for (id in accflow$HydroID) { cat(id, '\n')
                i=i+1
                #sum runoff from runoff table for all listed in flowdata
                v=strsplit(subflow[[i]],',') # isolate the list associated with HydroID
                ids=as.vector(v[[1]]) #turn the list into a vector
                tt=sum(runoff$runoff[which(runoff$node_id %in% ids)]) # find the vector in node_id, and sum the runoff values for all
                accflow$acc_runoff[which(accflow$HydroID==id)]=tt #print accumulated runoff to another table

}
)
#then write it out
