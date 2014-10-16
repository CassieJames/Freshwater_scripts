


args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

library(SDMTools); library(parallel)


source('/home/jc148322/scripts/libraries/cool_functions.r')
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
load(paste('/home/jc246980/SDM/Realized/',taxon,'/Clip4North/',spp, ".cur.real.mat.Rdata",sep=''))
pos=merge(pos,distdata, by='SegmentNo',all.x=TRUE)
save(pos,file=paste(out.dir,spp,'.cur.real.grid.Rdata',sep='')); rm(pos);gc() #write out the data		

