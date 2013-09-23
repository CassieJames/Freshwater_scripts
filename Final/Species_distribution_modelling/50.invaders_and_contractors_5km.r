
###### Script to determine numbers of invaders, contractors and retainers 
###### C. James (based on scripts by J VanDerWal and A. Reside	18 september 2013
################################################################################
	module load R 
	
	library(SDMTools)#load the necessary libraries
	library(parallel)
	source('/home/jc148322/scripts/libraries/cool_functions.r')

#### First future clip data to realised distributions and save out
	
	load('/home/jc148322/NARPfreshwater/SDM/clip.Rdata') #load position data of segmentNo and regions. object called clip.
	load('/home/jc148322/NARPfreshwater/SDM/connectivity.file.Rdata') #load position data of segmentNo and connected 'sub-graphs
	
	sdm.dir = '/home/jc148322/NARPfreshwater/SDM/'	
	taxa = c("fish", "crayfish","frog","turtles") ;	tax = taxa[3]
	work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
	out.dir="/home/jc246980/SDM/Realised_distributions/"
	
	exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
	exclude=exclude[which(exclude[,2]=='exclude'),1]
	species=list.files(work.dir)
	species=setdiff(species,exclude)
	if (tax==taxa[1]) clip.column='province' else clip.column='basins2'
	ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]
	
	for (spp in species) { print(spp)

		spp.dir=paste(work.dir,spp,'/',sep='');setwd(spp.dir)
		load(paste('summary/',es,'.pot.mat.Rdata',sep='')) #load the potential distribution data. object is called pot.mat
		thresdir = paste(work.dir,"/",spp,'/',sep='')
		threshold = read.csv(paste(thresdir,'output/maxentResults.csv',sep=''))#; threshold = threshold[which(threshold$Species==spp),]
		threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value	
		real.mat=pot.mat[,2:ncol(pot.mat)] #create a copy of potential matrix for clipping, drop SegmentNo (rebind later)
		real.mat[which(real.mat<=threshold)]=0 #clip anything below threshold to 0

		if(file.exists('summary/clip.csv')) {
		SegmentNo=read.csv('summary/clip.csv',as.is=TRUE)
		} else {

				occur=read.csv('occur.csv',as.is=T) #load occurrence data
				regions=unique(clip[which(clip$SegmentNo %in% occur$lat),clip.column])#find the regions in which species has been observed - this will be fish provinces for fish, and level 2 basins for other taxa
				SegmentNo=clip$SegmentNo[which(clip[,clip.column] %in% regions)] #find the segment nos within those regions

				real.mat[which(!(pot.mat[,'SegmentNo'] %in% SegmentNo)),]=0 #apply the clip
				SegmentNo=pot.mat[which(real.mat[,1]>0),1] #further limit the clip to streams connected to areas currently suitable - find all currently suitable SegmentNos

				catchments=unique(connectivity[which(connectivity[,1] %in% SegmentNo),2])#find the basins that have a suitable section currently
				SegmentNo=connectivity[which(connectivity[,2] %in% catchments),1] #find the segment nos within those catchments

				SegmentNo=as.data.frame(SegmentNo) #turn it into a data frame for saving ease		
		}
		
		real.mat[which(!(pot.mat[,'SegmentNo'] %in% SegmentNo[,1])),]=0 #apply the clip
		real.mat[which(real.mat>0)]=1
		save(real.mat,file=paste(out.dir,tax,"/",es,".",spp,'.real.mat.Rdata',sep='')); rm(pot.mat); gc() #write out the data
}

#### First current 1990 clip data to realised distributions and save out
	
	load('/home/jc148322/NARPfreshwater/SDM/clip.Rdata') #load position data of segmentNo and regions. object called clip.
	load('/home/jc148322/NARPfreshwater/SDM/connectivity.file.Rdata') #load position data of segmentNo and connected 'sub-graphs
	
	sdm.dir = '/home/jc148322/NARPfreshwater/SDM/'	
	taxa = c("fish", "crayfish","frog","turtles") ;	tax = taxa[3]
	work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
	out.dir="/home/jc246980/SDM/Realized_current/"
	exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
	exclude=exclude[which(exclude[,2]=='exclude'),1]
	species=list.files(work.dir)
	species=setdiff(species,exclude)
	if (tax==taxa[1]) clip.column='province' else clip.column='basins2'
	
	for (spp in species) { print(spp)

		distdata=read.csv(paste(work.dir,spp,"/output/potential/current_1990.csv",sep=''),as.is=T) #load the potential distribution current distribution
		cois=c(2,3) #limit current distribution to SegNo, Current
		distdata=distdata[,cois]; colnames(distdata)=c('SegmentNo',spp)
		
		
		thresdir = paste(work.dir,"/",spp,'/',sep='')
		threshold = read.csv(paste(thresdir,'output/maxentResults.csv',sep=''))#; threshold = threshold[which(threshold$Species==spp),]
		threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value	
		
		distdata[which(distdata[,2]<=threshold),2]=0 #clip anything below threshold to 0
		occur=read.csv(paste(work.dir,spp,'/occur.csv', sep=''),as.is=T) #load occurrence data
		regions=unique(clip[which(clip$SegmentNo %in% occur$lat),clip.column])#find the regions in which species has been observed - this will be fish provinces for fish, and level 2 basins for other taxa. Remember that occur$lat is actually SegmentNo
		SegmentNo=clip$SegmentNo[which(clip[,clip.column] %in% regions)] #find the segment nos within those regions
		distdata[which(!(distdata[,'SegmentNo'] %in% SegmentNo)),spp]=0 #apply the clip
		colnames(distdata)=c('SegmentNo','Current')
		save(distdata,file=paste(out.dir,tax,"/",spp,'.cur.real.mat.Rdata',sep='')); rm(distdata); gc() #write out the data		
	
	}
		

#### Determine contractors and invaders
	
	taxa = c("fish", "crayfish","frog","turtles")
	tax = taxa[3]	
	ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
	real.dir=paste("/home/jc246980/SDM/Realised_distributions/",tax,"/",sep="") 
	cur.dir=paste("/home/jc246980/SDM/Realized_current/",tax,"/",sep="") 
	sdm.dir = '/home/jc148322/NARPfreshwater/SDM/'	
	work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
	out.dir=paste("/home/jc246980/SDM/Invaders_contractors/",tax,"/Quantiles/",sep="")
	
	exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
	exclude=exclude[which(exclude[,2]=='exclude'),1]
	species=list.files(work.dir)
	species=setdiff(species,exclude)

	for (spp in species) { print(spp)

			load(paste(real.dir,es,".",spp,'.real.mat.Rdata',sep='')) #load the realised distribution data. object is called real.mat
			load(paste(cur.dir, spp,'.cur.real.mat.Rdata',sep='')) #load the realised distribution data. object is called distdata
			
			real.mat[which(real.mat>0)]=1 #clip anything below threshold to 0
			distdata[which(distdata[,2]>0),2]=1 #clip anything above threshold to 1

			Change_areas=real.mat[,2:145]-distdata[,2]
			new_areas = lost_areas = Change_areas
			new_areas[which(is.finite(new_areas) & new_areas<0)] = 0
			lost_areas[which(is.finite(lost_areas) & lost_areas>0)] = 0
			lost_areas[which(is.finite(lost_areas) & lost_areas<0)] = 1
			
			#save(new_areas,file=paste(out.dir,tax,"/","Invaders/",spp,'.new_areas.mat.Rdata',sep='')); rm(new_areas); gc() #write out the data
			#save(lost_areas,file=paste(out.dir,tax,"/","Contractors/",spp,'.lost_areas.mat.Rdata',sep='')); rm(lost_areas); gc() #write out the data
			#save(retain_areas,file=paste(out.dir,tax,"/","Retainers/",spp,'.retain_areas.mat.Rdata',sep='')); rm(retain_areas); gc() #write out the data				

			if(spp==species[1]) Invaders=new_areas else Invaders=Invaders+new_areas
			if(spp==species[1]) Contractors=lost_areas else Contractors=Contractors+lost_areas


			}
			save(Invaders,file=paste(out.dir,es,'.Invaders.mat.Rdata',sep='')); rm(Invaders)
			save(Contractors,file=paste(out.dir,es,'.Contractors.mat.Rdata',sep='')); rm(Contractors)


		
# determine quantiles
YEARs=seq(2015,2085,10)

outquant=NULL

data.dir=paste('/home/jc246980/SDM/Invaders_contractors/',tax,'/Quantiles/',sep=''); setwd(out.dir)
load(paste(data.dir,es,'.Invaders.mat.Rdata',sep='')) 
load(paste(data.dir,es,'.Contractors.mat.Rdata',sep='')) 

outquant_Invaders=NULL
outquant_Contractors=NULL

for (yr in YEARs) {

cois=grep(yr,colnames(Invaders))
tdata=Invaders[,cois]


ncore=8 #define number of cores
cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
tout = t(parApply(cl,tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
stopCluster(cl) #stop the cluster for analysis

###need to store the outputs
outquant_Invaders=cbind(outquant_Invaders,tout)

cois=grep(yr,colnames(Contractors))
tdata=Contractors[,cois]

ncore=8 #define number of cores
cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
tout = t(parApply(cl,tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
stopCluster(cl) #stop the cluster for analysis

###need to store the outputs
outquant_Contractors=cbind(outquant_Contractors,tout)

}

taxa = c("fish", "crayfish","frog","turtles"); tax=taxa[1]
out.dir = '/home/jc246980/SDM/Invaders_contractors/'
load('/home/jc148322/NARPfreshwater/SDM/models_fish/Acanthopagrus_australis/summary/RCP85.pot.mat.Rdata')

outquant_Invaders=cbind(pot.mat[,1],outquant_Invaders)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Invaders)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Invaders,file=paste(out.dir,es,"_",tax,'_Invaders.Rdata',sep=''))

outquant_Contractors=cbind(pot.mat[,1],outquant_Contractors)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Contractors)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Contractors,file=paste(out.dir,es,"_",tax,'_Contractors.Rdata',sep=''))





	

	
	