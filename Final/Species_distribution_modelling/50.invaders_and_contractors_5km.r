
###### Script to determine numbers of invaders, contractors and retainers 
###### C. James (based on scripts by J VanDerWal and A. Reside	18 september 2013
################################################################################
	module load R 
	
	
	library(SDMTools)#load the necessary libraries
	library(parallel)
	source('/home/jc148322/scripts/libraries/cool_functions.r')		

#### Determine contractors and invaders
	
	taxa = c("fish", "crayfish","frog","turtles")
	tax = taxa[1]	
	ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
	real.dir=paste("/home/jc246980/SDM/Realized/",tax,"/Clip4North/",sep="") 
	sdm.dir = '/home/jc246980/SDM/'		
	work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
	out.dir=paste("/home/jc246980/SDM/Invaders_contractors/",tax,"/Data/",sep="")
	
	exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
	exclude=exclude[which(exclude[,2]=='exclude'),1]
	species=list.files(real.dir, pattern=".Rdata")
	species=gsub(".cur.real.mat.Rdata", "", species)

for (es in ESs) {print(es)		
# determine contractors and invaders
for (spp in species) { print(spp)

			load(paste(real.dir,es,"/",spp,'.fut.real.mat.Rdata',sep='')) #load the future realised distribution data. object is called real.mat
			load(paste(real.dir,spp,'.cur.real.mat.Rdata',sep='')) #load the current realised distribution data. object is called distdata
			
			real.mat[which(real.mat>0)]=1 #clip anything above threshold to 1
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
}

### Determine quantiles for the future 

YEARs=seq(2015,2085,10)
data.dir=paste('/home/jc246980/SDM/Invaders_contractors/',tax,'/Data/',sep=''); setwd(data.dir)
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

taxa = c("fish", "crayfish","frog","turtles"); tax=taxa[4]
out.dir = '/home/jc246980/SDM/Invaders_contractors/'
load('/home/jc246980/SDM/models_fish/Ambassis_agassizii/summary/RCP85.pot.mat.Rdata')

outquant_Invaders=cbind(pot.mat[,1],outquant_Invaders)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Invaders)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Invaders,file=paste(out.dir,es,"_",tax,'_Invaders.Rdata',sep=''))

outquant_Contractors=cbind(pot.mat[,1],outquant_Contractors)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Contractors)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Contractors,file=paste(out.dir,es,"_",tax,'_Contractors.Rdata',sep=''))



##############################TURNOVER

	
library(SDMTools)#load the necessary libraries
library(parallel)
source('/home/jc148322/scripts/libraries/cool_functions.r')		
	
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[4]	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
out.dir=paste("/home/jc246980/SDM/TurnOver/",tax,"/",sep="")

load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
YEARs=seq(2015,2085,10)
data.dir=paste('/home/jc246980/SDM/Invaders_contractors/',tax,'/Data/',sep='')
load(paste(data.dir,es,'.Invaders.mat.Rdata',sep='')) 
load(paste(data.dir,es,'.Contractors.mat.Rdata',sep='')) 

Turnover=((Invaders+Contractors))/(Richness_current[,2])

save(Turnover,file=paste(out.dir,es,'.Turnover_noADD1.mat.Rdata',sep=''))

outquant_Turnover=NULL

for (yr in YEARs) {
	
	cois=grep(yr,colnames(Turnover))
	tdata=Turnover[,cois]

	ncore=8 #define number of cores
	cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
	tout = t(parApply(cl,tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
	stopCluster(cl) #stop the cluster for analysis

	###need to store the outputs
	outquant_Turnover=cbind(outquant_Turnover,tout)

}

out.dir = '/home/jc246980/SDM/TurnOver/'
load('/home/jc246980/SDM/models_fish/Ambassis_agassizii/summary/RCP85.pot.mat.Rdata')

outquant_Turnover=cbind(pot.mat[,1],outquant_Turnover)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Turnover)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Turnover,file=paste(out.dir,es,"_",tax,'_Turnover_quants_noADD1.Rdata',sep=''))




##############################Model agreement

	
library(SDMTools)#load the necessary libraries
library(parallel)
source('/home/jc148322/scripts/libraries/cool_functions.r')		
	
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[2]	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
out.dir=paste("/home/jc246980/SDM/Richness/Clip4North/",tax,"/",sep="")

load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'_Richness_future.mat.Rdata',sep='')) # Future
YEARs=seq(2015,2085,10)
yr=YEARs[8]

for (yr in YEARs) {
	
	cois=grep(yr,colnames(Richness_future))
	tdata=Richness_future[,cois]
	ttdata=(tdata+1)/(Richness_current[,2]+1)
	
	Increases_tdata=Decreases_tdata=ttdata
	
	Increases_tdata[which(Increases_tdata<1)] = 0
	Increases_tdata[which(Increases_tdata>1)] = 1
	Iout = apply(Increases_tdata,1,sum)
	
	Decreases_tdata[which(Decreases_tdata<1)] = 1
	Decreases_tdata[which(Decreases_tdata>1)] = 0
	Dout = apply(Decreases_tdata,1,sum)

	Model_agree=cbind(Richness_current[,1],Iout,Dout)
	No_change=	(Model_agree[,2]+Model_agree[,3])-18
	Model_agree=cbind(Model_agree, No_change)
	Model_agree[,2]=Model_agree[,2]-No_change
	Model_agree[,3]=Model_agree[,3]-No_change
	Model_agree[,3]=Model_agree[,3]*-1
	colnames(Model_agree)[1]<-"SegmentNo"
	colnames(Model_agree)[2]<-"Increases_agree"
	colnames(Model_agree)[3]<-"Decreases_agree"
	save(Model_agree,file=paste(out.dir,es,"_",yr,".Richness_agree.Rdata",sep=''))

}











