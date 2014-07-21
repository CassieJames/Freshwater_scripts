#### First current 1990 clip data to realised distributions and save out
#### C.James 	
	
################################################################################
###get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

################################################################################


load('/home/jc148322/NARPfreshwater/SDM/clip.Rdata') #load position data of segmentNo and regions. object called clip.
load('/home/jc148322/NARPfreshwater/SDM/connectivity.file.Rdata') #load position data of segmentNo and connected 'sub-graphs


distdata=read.csv(paste(wd,spp,"/output/potential/current_1990.csv",sep='')) #load the potential distribution current distribution
cois=c(2,3) #limit current distribution to SegNo, Current
distdata=distdata[,cois]; colnames(distdata)=c('SegmentNo',spp)

thresdir = paste(wd,"/",spp,'/',sep='')
threshold = read.csv(paste(thresdir,'output/maxentResults.csv',sep=''))#; threshold = threshold[which(threshold$Species==spp),]
threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value	

distdata[which(distdata[,2]<=threshold),2]=0 #clip anything below threshold to 0
occur=read.csv(paste(wd,spp,'/occur.csv', sep=''),as.is=T) #load occurrence data
regions=unique(clip[which(clip$SegmentNo %in% occur$lat),clip.column])#find the regions in which species has been observed - this will be fish provinces for fish, and level 2 basins for other taxa. Remember that occur$lat is actually SegmentNo
SegmentNo=clip$SegmentNo[which(clip[,clip.column] %in% regions)] #find the segment nos within those regions
distdata[which(!(distdata[,'SegmentNo'] %in% SegmentNo)),spp]=0 #apply the clip
colnames(distdata)=c('SegmentNo','Current')
save(distdata,file=paste(out,"/",spp,'.cur.real.mat.Rdata',sep='')); rm(distdata); gc() #write out the data		
	
