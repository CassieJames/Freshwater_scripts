
# script to determine which species gain under 2085 median climate scenario

taxa = c("fish", "crayfish","frog","turtles"); tax = taxa[1]	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]
out.dir=paste("/home/jc246980/SDM/Richness/Clip4North/",tax,"/",sep="")
sh.dir='/home/jc246980/SDM/Richness/temp/'; setwd(sh.dir)
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/30.Script2run.r'

exclude=read.csv('/home/jc148322/NARPfreshwater/SDM/fish.to.exclude.csv',as.is=TRUE)
exclude=exclude[which(exclude[,2]=='exclude'),1]
species=list.files(real.dir)
species.name=sub(".fut.real.mat.Rdata","", species)	
species=setdiff(species.name,exclude)

for (spp in species) { print(spp)
	load(paste(cur.dir, spp,'.cur.real.mat.Rdata',sep='')) #load the current realised distribution data. 
	distdata[which(distdata[,2]>0),2]=1 #clip anything above threshold to 1
	load(paste(real.dir,spp,'.fut.real.mat.Rdata',sep='')) #load the future realised distribution data. object is called real.mat
	fut.mat=real.mat[,2:145]
	yr=2085
	cois=grep(yr,colnames(fut.mat))
	tdata=fut.mat[,cois]
	ncore=8 #define number of cores
	cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
	tout = t(parApply(cl,tdata,1,function(x) { return(quantile(x,c(0.5),na.rm=TRUE,type=8)) }))
	stopCluster(cl) #stop the cluster for analysis
	Fout=cbind(distdata,tout)
	}
	colnames(Fout)=c("SegmentNo", species)
	
catchments = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),catchments)
RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc) # Map river basins onto position file
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector
outdelta=merge(pos,Fout,by='SegmentNo',all.x=T)


