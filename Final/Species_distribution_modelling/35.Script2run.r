
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

library(parallel)

YEARs=seq(2015,2085,10)
outquant_Richness=NULL
load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,"/",es,'_Richness_future.mat.Rdata',sep=''))

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


load('/home/jc246980/SDM/models_fish/Ambassis_agassizii/summary/RCP85.pot.mat.Rdata')
outquant_Richness=cbind(pot.mat[,1],outquant_Richness)
tt=expand.grid(c(10,50,90),YEARs)
colnames(outquant_Richness)=c('SegmentNo',paste(tt[,2],'_',tt[,1],sep=''))
save(outquant_Richness,file=paste(out.dir,es,".Richness_quants.Rdata",sep=''))







