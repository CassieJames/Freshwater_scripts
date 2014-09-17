### Script to determine richness and create quantiles
### C James


args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments
library(SDMTools)


exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
exclude=exclude[which(exclude[,2]=='exclude'),1]
species=list.files(real.dir)
species.name=sub(".fut.real.mat.Rdata","", species)	
species=setdiff(species.name,exclude)
	
	
for (spp in species) { print(spp)

# Determine current species richness

	load(paste(cur.dir, spp,'.cur.real.mat.Rdata',sep='')) #load the current realised distribution data. 
	distdata[which(distdata[,2]>0),2]=1 #clip anything above threshold to 1
	if(spp==species[1]) distdata=distdata else distdata[,'SegmentNo'] = 0
	if(spp==species[1]) Richness_current=distdata[,1:2] else Richness_current=Richness_current+distdata [,1:2]

# determine future species richness

	# load(paste(real.dir,spp,'.fut.real.mat.Rdata',sep='')) #load the future realised distribution data. object is called real.mat
	# fut.mat=real.mat[,2:145]
	# fut.mat[which(fut.mat>0)]=1 #clip anything above threshold to 1
	# fut.mat=cbind(real.mat[,"SegmentNo"], fut.mat)
	# colnames(fut.mat)[1]="SegmentNo"
	# if(spp==species[1]) fut.mat=fut.mat else fut.mat[,'SegmentNo'] = 0
	# if(spp==species[1]) Richness_future=fut.mat else Richness_future=Richness_future+fut.mat

}

save(Richness_current,file=paste(out.dir,es,'_Richness_current.mat.Rdata',sep=''));rm(Richness_current)
save(Richness_future,file=paste(out.dir,es,'_Richness_future.mat.Rdata',sep='')); rm(Richness_future)	
