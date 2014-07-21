###get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments


##get the delta data in the same format as cassie's delta outputs in preparation for input into her stability summary script

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
out.dir="/home/jc148322/NARPfreshwater/SDM/richness/summaries/deltas/"; dir.create(out.dir,recursive=T)

ESs=list.files(future.dir, pattern='RCP')

###01. make a pos file with Segment No, River basins, and Ramsars
catchments = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),catchments)

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc) # Map river basins onto postition file
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

Ramsar.asc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load river basin asc
pos$ramsar = extract.data(cbind(pos$lon,pos$lat), Ramsar.asc) # Map river basins onto postition file
RAMSARS = unique(na.omit(pos$ramsar)) # create river basin vector

basedir='/home/jc148322/NARPfreshwater/SDM/richness/'
taxa=c('fish','crayfish','frog','turtles')

###02. for each taxa, load the percentile richness and cbind the data for all years and all RCPs together. label columns as es_yr_percentile

for (es in ESs) { cat(es,'\n') #cycle through each variable of interest
        load(paste(basedir,tax,'/',es,'_richness.Rdata',sep='')) #load the data
        cois=c(3:ncol(outquant))
        colnames(outquant)[cois]=paste(es,'_',colnames(outquant)[cois],sep='')
        if (es==ESs[1]) out=outquant else out=cbind(out, outquant[,3:ncol(outquant)])
        
        tout=outquant[,cois]
        tout=tout/outquant[,'current']
        tout[which(is.nan(tout))]=NA
        tout[which(tout>2)]=2
        if (es==ESs[1]) outdelta=cbind(outquant[,1:2],tout) else outdelta=cbind(outdelta, tout)
        
}
###03. merge the percentile richness with pos for each taxa and save the data out
out=merge(pos,out,by='SegmentNo',all.x=T)
outdelta=merge(pos,outdelta,by='SegmentNo',all.x=T)
setwd(out.dir)


save(out,outdelta, file=paste(tax,'_delta.Rdata', sep=''))

