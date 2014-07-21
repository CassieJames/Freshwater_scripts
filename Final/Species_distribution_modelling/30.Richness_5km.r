
#### Determine current and future species richness
	library(parallel)
	
	taxa = c("fish", "crayfish","frog","turtles")
	tax = taxa[4]	
	ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
	
	real.dir=paste("/home/jc246980/SDM/Realised_distributions/",tax,"/",sep="") # Futures!
	cur.dir=paste("/home/jc246980/SDM/Realized_current/",tax,"/",sep="") 
	sdm.dir = '/home/jc246980/SDM/'		
	work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
	out.dir=paste("/home/jc246980/SDM/Richness/",tax,sep="")
	
	exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
	exclude=exclude[which(exclude[,2]=='exclude'),1]
	species=list.files(work.dir)
	species=setdiff(species,exclude)

	
		
# determine current species richness
for (spp in species) { print(spp)

			load(paste(cur.dir, spp,'.cur.real.mat.Rdata',sep='')) #load the current realised distribution data. object is called distdata
			distdata[which(distdata[,2]>0),2]=1 #clip anything above threshold to 1
			if(spp==species[1]) distdata=distdata else distdata[,'SegmentNo'] = 0
			if(spp==species[1]) Richness_current=distdata else Richness_current=Richness_current+distdata

			}
save(Richness_current,file=paste(out.dir,'Richness_current.mat.Rdata',sep=''))
		

# determine future species richness

for (spp in species) { print(spp)
	load(paste(real.dir,es,".",spp,'.real.mat.Rdata',sep='')) #load the future realised distribution data. object is called real.mat
	real.mat[which(real.mat>0)]=1 #clip anything above threshold to 1
	if(spp==species[1]) Richness_future=real.mat else Richness_future=Richness_future+real.mat

	}
save(Richness_future,file=paste(out.dir,es,'.Richness_future.mat.Rdata',sep=''))	


# deteremine qantiles of future richness

out.dir = '/home/jc246980/SDM/Richness/'; setwd(out.dir)
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[4]	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
YEARs=seq(2015,2085,10)
outquant_Richness=NULL
load(paste('/home/jc246980/SDM/Richness/',es,".",tax,'Richness_future.mat.Rdata',sep=''))

for (yr in YEARs) {
	
	cois=grep(yr,colnames(Richness_future))
	tdata=Richness_future[,cois]


	ncore=8 #define number of cores
	cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
	tout = t(parApply(cl,tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
	stopCluster(cl) #stop the cluster for analysis

	###need to store the outputs
	outquant_Richness=cbind(outquant_Richness,tout)


}

taxa = c("fish", "crayfish","frog","turtles"); tax=taxa[4]
load('/home/jc246980/SDM/models_fish/Ambassis_agassizii/summary/RCP85.pot.mat.Rdata')

outquant_Richness=cbind(pot.mat[,1],outquant_Richness)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Richness)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Richness,file=paste(out.dir,es,"_",tax,'_Richness_quants.Rdata',sep=''))






