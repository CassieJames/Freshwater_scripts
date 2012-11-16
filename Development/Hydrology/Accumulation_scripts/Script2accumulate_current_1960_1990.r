#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(igraph); library(parallel) #load the necessary libraries

#define inputs
input.dir='/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/'
load(file=paste(input.dir,'Current_dynamic.Rdata',sep=''))
attribute=Runoff #rename object
attribute$SegmentNo=1:1466889

cois=colnames(attribute)[-grep('SegmentNo',colnames(attribute))] #define a vector of your colnames of interest

#define outputs
out.dir='/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1960to1990/' #define output directory
filename='Current_dynamic_accumulated' #name your file
################################################################################

wd = "/home/jc165798/working/NARP_hydro/flow_accumulation/"; setwd(wd) #define and set working directory

###read in necessary data
network = read.csv('NetworkAttributes.csv',as.is=TRUE) #read in the data
proportion = read.csv('proportion.csv',as.is=TRUE)

#prepare all data
db = merge(network,proportion[,c(1,4,5)],all=TRUE) #read in proportion rules and merge with network data
attribute=as.data.frame(attribute) #convert attributes to dataframe
attribute=attribute[which(attribute$SegmentNo %in% network$SegmentNo),] #remove extra SegmentNos
attribute=na.omit(attribute)
db = merge(db,attribute,all=TRUE) #merge data into db
db[,cois]=db[,cois]*db$SegProp #Calculate local attribute attributed to each HydroID and overwrite SegNo attribute
db = db[,c(11,12,1:10,13:ncol(db))] #reorder the columns
db=db[which(is.finite(db[,cois[1]])),] #remove NAs (islands, etc)
rm(list=c("network","attribute","proportion")) #cleanup extra files

### create graph object and all possible subgraphs
g = graph.data.frame(db,directed=TRUE) #create the graph
gg = decompose.graph(g,"weak") #break the full graph into 10000 + subgraphs

### do the accumulation
#function to accumulate info in each subgraph in a full graph
accum = function(gt,cois) {
require(igraph)
out=NULL #define the output
while(length(E(gt))>0) { #loop until all edges are dealt with
vois = which(degree(gt,mode="in")==0) #get index of the headwater vertices
suppressWarnings({
tt = do.call("rbind", neighborhood(gt,1,V(gt)[vois],"out")) #get the index of the from & to nodes for each edge in a list
})
v.from.to = NULL; for (ii in 2:ncol(tt)) v.from.to = rbind(v.from.to,tt[,c(1,ii)]) #flatten the list to a matrix and setup for next cleaning
v.from.to = rbind(v.from.to,v.from.to) #allow for duplicate from-to nodes with separate SegmentNo
if (is.null(dim(v.from.to))) v.from.to = matrix(v.from.to,ncol=2) #ensure v.from.to is a matrix
eois = get.edge.ids(gt,t(cbind(V(gt)[v.from.to[,1]],V(gt)[v.from.to[,2]])),multi=TRUE) #get an index of the output edges from that vertex
eois = unique(eois); if (0 %in% eois) eois = eois[-which(eois==0)] #only keep unique eois
tout=E(gt)$HydroID[eois] #prepare empty df to store attributes
for (coi in cois){ tt=get.edge.attribute(gt,coi,eois); tout=cbind(tout,tt)} #store the attribute for the selected edges for each of the columns to be accumulated
colnames(tout)=c('HydroID',cois) #name the columns
out = rbind(out,tout) #store the attribute for the current edges

suppressWarnings({
tt = cbind(eois, do.call("rbind", neighborhood(gt,1,V(gt)[v.from.to[,2]],"out"))) #get the next down verticies from the current edges
})
if ((length(dim(tt))<1 & length(tt)>2) | (length(dim(tt))>0 & ncol(tt)>2)) { #only do this if there is something down stream
next_edge = NULL; for (ii in 3:ncol(tt)) next_edge = rbind(next_edge,tt[,c(1,2,ii)]) #flatten the list to a matrix and setup for next cleaning
#next_edge = unique(next_edge); next_edge = next_edge[which(next_edge[,2]-next_edge[,3]!=0),] #remove duplicated and where from and to are the same
if (is.null(dim(next_edge))) next_edge = matrix(next_edge,ncol=3) #ensure v.from.to is a matrix
next_edge = cbind(next_edge,get.edge.ids(gt,t(cbind(V(gt)[next_edge[,2]],V(gt)[next_edge[,3]])),multi=T)) #get an index of the next down edges
next_edge=unique(next_edge); next_edge=next_edge[which(next_edge[,4]!=0),]
next_edge=matrix(next_edge,ncol=4); colnames(next_edge) = c("e.from","from","to","e.next")
for (coi in cois) {
v=get.edge.attribute(gt, coi, next_edge[,'e.next']) + E(gt)$BiProp[next_edge[,"e.next"]] * get.edge.attribute(gt, coi, next_edge[,'e.from'])
gt=set.edge.attribute(gt, coi, next_edge[,'e.next'],v)
}
}
gt = delete.vertices(gt, V(gt)[vois]) #remove the vois
}
return(out)
}

###do the actual accumulation
ncore=4 #this number of cores seems most appropriate
cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
print(system.time({ tout = parLapplyLB(cl,gg,accum, cois=cois) }))
stopCluster(cl) #stop the cluster for analysis

###need to store the outputs
out = do.call("rbind",tout) #aggregate the list into a single matrix
db2 = merge(db,out) #merge this back into the overall database

write.csv(out,paste(out.dir,filename,'.csv',sep=''),row.names=F)