
library(SDMTools)
library(maptools) 
library(igraph)
library(parallel) #load the necessary libraries#define the libraries needed
source('/home/jc148322/scripts/libraries/cool_functions.r')
### set up classes in case they are useful

load("/home/jc246980/Obsolete/Hydrology.trials/Catchmentraster250.Rdata")
Hydro.class=read.csv(paste("/home/jc246980/Janet_Stein_data/Hydro_class.csv",sep=''))	
Hydro.class.long=Hydro.class$Longitude
Hydro.class.lat=Hydro.class$Latitude
SegmentNo_Hydrology_class  = extract.data(cbind(Hydro.class.long,Hydro.class.lat),CatchmentRaster.asc) # extract Segment number from Catchment raster

seg4hydroclass=as.data.frame(SegmentNo_Hydrology_class)

seg4hydroclass=cbind(Hydro.class, seg4hydroclass)
seg4hydroclass=seg4hydroclass[, c(10,9)]
colnames(seg4hydroclass)=c("SegmentNo", "Hydro_class")

network.file="/home/jc246980/Janet_Stein_data/NetworkAttributes.csv"
network = read.csv(network.file,as.is=TRUE) 
db = network[,c(11,12,1:10,13:ncol(network))] #reorder the columns

terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')
cois=c('SEGMENTNO', 'VALLEYSLOP', 'CATSLOPE','D2OUTLET', "STRAHLER")
terrain_sub=terrain[,cois]
colnames(terrain_sub)=c("SegmentNo", "Segslope", "Catslope", "d2outlet", "STRAHLER")
db=merge(db,terrain_sub, by='SegmentNo',all.x=TRUE)

g = graph.data.frame(db,directed=TRUE) #create the graph
gg = decompose.graph(g,"weak") #break the full graph into 10000 + subgraphs

network.abr=network[, c(10,11,12)]

out=NULL
for (i in seq_along(gg)) { cat(i,'\n') #For each subgraph comps[[i]]
	
	gg.edges <- as.data.frame((get.edgelist(gg[[i]])))
	gg.edges=as.data.frame(gg.edges[,1])
	colnames(gg.edges)="From_Node"
	tdat=merge(network.abr,gg.edges, by='From_Node',all.y=TRUE)	
	tdat$Stream_net=i
	if (i==1) {out=tdat}
	if (i>=2) {out=rbind(out, tdat)}
}

stream_network_pos = out[,c(2,1,3,4)] #reorder the columns
save(stream_network_pos,file='/home/jc246980/Janet_Stein_data/stream_network_pos.Rdata') #write out the data

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
pos=merge(pos,stream_network_pos, by='SegmentNo',all.x=TRUE)
tasc=make.asc(pos[,'Stream_net'])
write.asc(tasc,'Stream_net.asc')

gauged_networks=unique(stream_network_pos[which((stream_network_pos[,'SegmentNo'] %in% SegmentNo_Hydrology_class)),'Stream_net']) # this gives list of gauged networks
soi=stream_network_pos[which((stream_network_pos[,'Stream_net'] %in% gauged_networks)),] # this gives a data frame which includes only networks that contain gauges

for (i in seq_along(gauged_networks)) { cat(i,'\n') #For each gauged network...
doi=soi[which((soi[,'Stream_net'] == i)),] # subset data to network of interest only
gauged_segs=doi[which((doi[,'SegmentNo'] %in% SegmentNo_Hydrology_class)),] # extract gauged segments within that network
out=NULL
	for (ii in seq_along(gauged_segs)) { # for each gauged segment within the network
	stahler=doi[which((doi[,'SegmentNo'] == ii)),"STRAHLER"] # extract strahler data for gauged segment
	sub_doi=doi[which((doi[,'STRAHLER'] == strahler)),] # subset network to data where strahler level is the same as that of the gauged segments
	sub_doi=merge(sub_doi,seg4hydroclass, by='SegmentNo',all.x=TRUE) # merge segments of interest with hydro class  info
	if (ii==gauged_segs[1]) {out=sub_doi}
	if (ii!=gauged_segs[1]) {out=rbind(out, sub_doi)}
	}
	
if (i==gauged_networks[1]) {out_final=out}
if (ii!=gauged_networks[1]) {out_final=rbind(out_final, out)}
	
}

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
pos=merge(pos,stream_network_pos, by='SegmentNo',all.x=TRUE)
tasc=make.asc(pos[,'Stream_net'])
write.asc(tasc,'extrapolated_stream_classes.asc')

