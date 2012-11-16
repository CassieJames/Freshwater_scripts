#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
#required to run ... module load R-2.15.1

### read in the necessary info
args=(commandArgs(TRUE)) #get the command line arguements
for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

### sample data
proportionate.accumulation=TRUE #false is accumulate area; true is accumulation runnoff
wd = "/home/jc246980/Hydrology.trials/Flow_accumulation/" #define the working directory
data.file="/home/jc246980/Hydrology.trials/Flow_accumulation/runoff4accumulations.csv" #define the name of the data file
network.file="/home/jc165798/working/NARP_hydro/flow_accumulation/NetworkAttributes.csv" #define the name of the network attribute data file
proportion.file="/home/jc165798/working/NARP_hydro/flow_accumulation/proportion.csv" #define the name of the proportionate attribute data file
accum.function.file="/home/jc165798/SCRIPTS/git_code/NCCARF_freshwater_refugia/hydrology/dev/accumulate_functions.R" #define the location of the accumulation functions

################################################################################
library(igraph); library(parallel) #load the necessary libraries
source(accum.function.file) #source the accumulation functions
setwd(wd) #define and set working directory

###read in necessary data
network = read.csv(network.file,as.is=TRUE) #read in the netowrk attribute data
proportion = read.csv(proportion.file,as.is=TRUE) #read in the proportionate data
stream.data = read.csv(data.file,as.is=TRUE) #read in the stream data to be summarized

#prepare all data
db = merge(network,proportion[,c(1,4,5)],all=TRUE) #read in proportion rules and merge with network data
cois=colnames(stream.data)[-grep('SegmentNo',colnames(stream.data))] #define a vector of your colnames of interest
stream.data=as.data.frame(stream.data) #convert attributes to dataframe
stream.data=na.omit(stream.data[which(stream.data$SegmentNo %in% stream.data$SegmentNo),]) #remove extra SegmentNos and missing data
db = merge(db,stream.data,all=TRUE) #merge data into db
db[,cois]=db[,cois]*db$SegProp #Calculate local attribute attributed to each HydroID and overwrite SegNo attribute
db = db[,c(11,12,1:10,13:ncol(db))] #reorder the columns
db=db[which(is.finite(db[,cois[1]])),] #remove NAs (islands, etc)
if (proportionate.accumulation==FALSE) db$BiProp=1
rm(list=c("network","stream.data","proportion")) #cleanup extra files

### create graph object and all possible subgraphs
g = graph.data.frame(db,directed=TRUE) #create the graph
gg = decompose.graph(g,"weak") #break the full graph into 10000 + subgraphs

###runoff accumulation
###do the actual accumulation
ncore=5 #this number of cores seems most appropriate
cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
print(system.time({ tout = parLapplyLB(cl,gg,accum.runoff, cois=cois) }))
stopCluster(cl) #stop the cluster for analysis

###need to store the outputs
out = do.call("rbind",tout) #aggregate the list into a single matrix
db2 = merge(db,out) #merge this back into the overall database

write.csv(out,paste(wd,'annual_flow_static_dynamic.csv',sep=''),row.names=F)

###################################################################################################
###bring in Janets data to test

network = read.csv(network.file,as.is=TRUE) #read in the netowrk attribute data
networkids=network[,c(2,9)]

streamatts=read.dbf("/home/jc246980/Janet_Stein_data/streamattributes.dbf")
Annualmean = streamatts[,2:3]

tdata=merge(out,networkids, by='HydroID')
Flow_agg_dynamo= aggregate(tdata$dynamo, by = list(tdata$SegmentNo), sum)      # sum flow at duplicate segment numbers
Flow_agg_static= aggregate(tdata$static, by = list(tdata$SegmentNo), sum)      # sum flow at duplicate segment numbers

colnames(Flow_agg_dynamo)=c("SEGMENTNO", "Dynamo")
colnames(Flow_agg_static)=c("SEGMENTNO", "Static")

Acc_data=merge(Flow_agg_static, Flow_agg_dynamo, by='SEGMENTNO')

Acc_data=merge(Acc_data,Annualmean, by='SEGMENTNO')

Acc_data$Janet_static=Acc_data[,4]/Acc_data[,2]
Acc_data$Janet_dynamo=Acc_data[,4]/Acc_data[,3]

setwd(wd)
library(SDMTools) #load the necessary library
wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz');
baseasc=base.asc

wd = "/home/jc246980/Hydrology.trials/Flow_accumulation/" #define the working directory

png(paste(wd,"Differences between Janets and our annual runoff.png", sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
par(mfrow=c(1,2),mar=c(5,5,2,1), oma=c(0,0,0,0)) 	

hist(log(Acc_data[,5]))
mtext('Janets/Static annual mean', line=3,  side=1, cex=1.5,font=2) 


hist(log(Acc_data[,6]))
mtext('Janets/Dynamic annual mean', line=3,  side=1, cex=1.5,font=2)  	 	
dev.off() #close out the image
	
