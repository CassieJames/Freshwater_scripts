datadir="/home/jc246980/Obsolete/Hydrology.trials/Stability/Runoff/"
outdir="/home/jc246980/Obsolete/Hydrology.trials/BEN_FORD/"
ESs=c("RCP45", "RCP85")

YOIS=c(2035, 2085)
MOI=c("csiro-mk30", "ccsr-miroc32med")

tdata=load(paste(datadir,"Data/",es,"_data_runoff.Rdata", sep=''))
tdata=data.runoff
outdata2 = matrix(NA,nrow=nrow(tdata),ncol=length(YOIS)*length(MOI)); #define the output matrix
tt = expand.grid(MOI,YOIS); tt = paste(tt[,2],tt[,1],"RCP85",sep='_'); colnames(outdata2) = tt #add the column names

for (yr in YOIS) {
for (moi in MOI) {
mydate=tdata[,intersect(grep(yr,colnames(tdata)),grep(moi,colnames(tdata)))]
runoff.asc=base.asc; runoff.asc[cbind(pos$row,pos$col)]=mydate
write.asc(runoff.asc,paste(outdir,es,"_",yr,"_",moi,"_runoff.asc",sep=''))
#outdata2[,intersect(grep(yr,colnames(outdata)),grep(moi,colnames(outdata)))] = mydate

}}


paste(outdir,es,"_",yr,"_",moi,"_runoff.asc",sep='')
runoff_RCP45=outdata
runoff_RCP85=outdata2

write.csv(runoff_RCP45,paste(outdir,'runoff_RCP45.csv',sep=''))
write.csv(runoff_RCP45,paste(outdir,'runoff_RCP85.csv',sep=''))

#### Create ascs base needed for above runs


library(SDMTools) #define the libraries needed

##########set up current climate baseline and attribute identifier
wd = '/home/jc246980/Obsolete/Climate/Baseline_5km/'; setwd(wd)   			  # define and set working directory
base.asc = read.asc('base.asc')                                               # Import base asc at 5 km resolution
tasc=base.asc                                                                 # Rename baseasc  at 5 km resolution for appending 
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution


##################################################################################################
### Runoff accumulated
datadir="/home/jc246980/SDM/Environmental_futures_final/"


raster=read.asc.gz('/home/jc246980/Obsolete/Climate/Baseline_1km/SegmentNo_1km.asc.gz')
pos = as.data.frame(which(is.finite(raster),arr.ind=T))
pos$lat = getXYcoords(raster)$y[pos$col]
pos$lon = getXYcoords(raster)$x[pos$row] #append the lat lon
pos$SegmentNo=raster[cbind(pos$row,pos$col)]    
base.asc=raster   

for (yr in YOIS) {
for (moi in MOI) {
for (es in ESs)  {
tdata=read.csv(paste(datadir, es,"_", moi,"_",yr,".csv",sep=""))
tpos=pos
tpos=merge(tpos,tdata,by='SegmentNo',all.x=TRUE)
tasc=base.asc; tasc[cbind(tpos$row,tpos$col)]=tpos$Flow_accum_annual
write.asc(tasc,paste(outdir,es,"_",yr,"_",moi,"_runoff_accumulated.asc",sep=''))

}}}