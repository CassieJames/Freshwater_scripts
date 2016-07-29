# Script to determine proportional change in suitable area between current and 2085

library(SDMTools)
library(maptools) 
library(foreign)
library(parallel)


taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] #change as appropriate
image.dir=paste('/home/jc246980/SDM/Realized/Images/',tax,'/Clip4North/',sep='')
wd=paste('/home/jc246980/SDM/Realized/',tax,'/Clip4North/',sep=''); setwd(wd)
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]
real.dir=paste("/home/jc246980/SDM/Realized/",tax,"/Clip4North/",es,"/",sep="") 

files=list.files(wd)
species=files[grep('cur.real.mat',files)]

Results=matrix(NA,nr=length(species),nc=2)
colnames(Results)=c("Species","Length")


netatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf') # 
cois=c('SegmentNo', 'Shape_Leng')
netatts_sub=netatts[,cois]
	
# Determine area currently suitable for each species

i=0
for (spp in species) {

species.name=sub(".cur.real.mat.Rdata","", spp)	
load(paste(wd,spp, sep='')) # Current		
distdata[which(distdata[,2]>0),2]=1 #clip anything above threshold to 1
tdata = merge(distdata,netatts_sub,by='SegmentNo')
result=sum(tdata$Current*tdata$Shape_Leng)
i=i+1
Results[i,1]=species.name
Results[i,2]=result
	}
write.csv(out,paste('/home/jc246980/SDM/Paper_images/',tax,'_Current_occupied_length.csv',sep=''), row.names=FALSE)


# Determine area suitable for each species in the future

Results_future=matrix(NA,nr=length(species),nc=2)
colnames(Results_future)=c("Species","Length")
yr=2085
i=0
for (spp in species) {
	species.name=sub(".cur.real.mat.Rdata","", spp)	
	load(paste(real.dir,species.name,'.fut.real.mat.Rdata',sep='')) #load the future realised distribution data. object is called real.mat
	cois=grep(yr,colnames(real.mat))
	tdata=real.mat[,cois] # returns 18 models for 2085 only
	rmeds <- apply(tdata, 1, median)  
	rmeds[which(rmeds>0)]=1 #clip anything above threshold to 1
	rmeds=as.data.frame(rmeds)
	rmeds=cbind(real.mat[,1],rmeds)
	colnames(rmeds)=c("SegmentNo","median")
	tdata = merge(rmeds,netatts_sub,by='SegmentNo')
	result=sum(tdata$median*tdata$Shape_Leng)
	i=i+1
	Results_future[i,1]=species.name
	Results_future[i,2]=result
	}
	
	
	