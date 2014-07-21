args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

library(SDMTools); library(parallel)
source('/home/jc148322/scripts/libraries/cool_functions.r')

#working directory
setwd(wd)
load('/home/jc246980/SDM/clipnew.Rdata') #load position data of segmentNo and regions. object called clip.
clip=clipnew
load('/home/jc148322/NARPfreshwater/SDM/connectivity.file.Rdata') #load position data of segmentNo and connected 'sub-graphs'. object called connectivity.

exclude=read.csv('/home/jc246980/SDM/fish.to.exclude.csv',as.is=TRUE)
exclude=exclude[which(exclude[,2]=='exclude'),1]
species=list.files(wd)
species=setdiff(species,exclude)



for (spp in species) {cat (spp,'\n')
#working directory
spp.dir=paste(wd,spp,'/',sep='');setwd(spp.dir)
threshold=read.csv('output/maxentResults.csv',as.is=TRUE) #read in the data for threshold
threshold=threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold #isolate threshold


##01. Clip to threshold
load(paste('summary/',es,'.pot.mat.Rdata',sep='')) #load the potential distribution data. object is called pot.mat

real.mat=pot.mat[,3:ncol(pot.mat)] #create a copy of potential matrix for clipping, drop SegmentNo (rebind later) and current
real.mat[which(real.mat<=threshold)]=0 #clip anything below threshold to 0

#occur=read.csv('occur.csv',as.is=T) #load occurrence data - not usng this as the current distrbutions have been substantially modified during vetting
load(paste(current,"/",spp,'.cur.real.mat.Rdata', sep='')) # load current realised distribution
occur=distdata[which(distdata$Current>0),] #remove SegmentNos (rows) with no occurrence records 

regions=(clip[which(clip$SegmentNo %in% occur$SegmentNo),clip.column])#find the regions in which species has been observed - this will be fish provinces for fish, and level 2 basins for other taxa
regions=as.data.frame(table(regions))
regions2=regions[regions$Freq >250,] # Some segments overlap border with adjacent provinces so need to remove these overlapped provinces
regions=regions2$regions
SegmentNo=clip$SegmentNo[which(clip[,clip.column] %in% regions)] #find the segment nos within those regions

real.mat[which(!(pot.mat[,'SegmentNo'] %in% SegmentNo)),]=0 #apply the clip
SegmentNo=pot.mat[which(real.mat[,1]>0),1] #further limit the clip to streams connected to areas currently suitable - find all currently suitable SegmentNos

catchments=unique(connectivity[which(connectivity[,1] %in% SegmentNo),2])#find the basins that have a suitable section currently
SegmentNo=connectivity[which(connectivity[,2] %in% catchments),1] #find the segment nos within those catchments

SegmentNo=as.data.frame(SegmentNo) #turn it into a data frame for saving ease
write.csv(SegmentNo,'summary/clip.csv',row.names=FALSE) #save
# }

real.mat[which(!(pot.mat[,'SegmentNo'] %in% SegmentNo[,1])),]=0 #apply the clip
real.mat[which(real.mat>0)]=1
real.mat=cbind(pot.mat[,1],real.mat)
colnames(real.mat)[1]=c('SegmentNo')
save(real.mat,file=paste(out,"/",es,".",spp,'.cur.real.mat.Rdata',sep='')); rm(real.mat); gc() #write out the data		


}