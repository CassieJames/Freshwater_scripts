# deteremine number of models predicting increases and decreases in richness relative to current

out.dir = '/home/jc246980/SDM/Richness/'; setwd(out.dir)
taxa = c("fish", "crayfish","frog","turtles")	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]	
YEARs=seq(2015,2085,10)

library(parallel)

myfun = function(x) {
tsum=sum(x > 1,na.rm=TRUE)
return(c(tsum)) #return the values
}

for (tax in taxa) {	print(tax)

load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
NumGCMs= matrix(NA,nrow=nrow(Richness_current),ncol=9)
colnames(NumGCMs)=c("SegmentNo", "yr_2015","yr_2025", "yr_2035", "yr_2045", "yr_2055", "yr_2065", "yr_2075", "yr_2085") 	
NumGCMs[,1]=Richness_current[,1]

load(paste('/home/jc246980/SDM/Richness/',es,".",tax,'Richness_future.mat.Rdata',sep=''))
load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
	
	for (yr in YEARs) {
		
		cois=grep(yr,colnames(Richness_future))
		tdata=Richness_future[,cois]
		tdata=tdata/Richness_current[,2]

		ncore=8 #define number of cores
		cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
		tout = t(parApply(cl,tdata,1,myfun))		
		stopCluster(cl) #stop the cluster for analysis
		###need to store the outputs
		NumGCMs[,(grep(yr,colnames(NumGCMs)))]=tout[,]
	}

save(NumGCMs,file=paste(out.dir,es,"_",tax,'_Increased_Rich_GCMs.Rdata',sep=''))

}

myfun = function(x) {
x[which(x==0)]=NA
tsum=sum(x < 1,na.rm=TRUE)
return(c(tsum)) #return the values
}

for (tax in taxa) {	print(tax)

load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
NumGCMs_d= matrix(NA,nrow=nrow(Richness_current),ncol=9)
colnames(NumGCMs_d)=c("SegmentNo", "yr_2015","yr_2025", "yr_2035", "yr_2045", "yr_2055", "yr_2065", "yr_2075", "yr_2085") 	
NumGCMs_d[,1]=Richness_current[,1]

load(paste('/home/jc246980/SDM/Richness/',es,".",tax,'Richness_future.mat.Rdata',sep=''))
load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	
	
	for (yr in YEARs) {
		
		cois=grep(yr,colnames(Richness_future))
		tdata=Richness_future[,cois]
		tdata=tdata/Richness_current[,2]

		ncore=8 #define number of cores
		cl <- makeCluster(getOption("cl.cores", ncore))#define the cluster for running the analysis
		tout = t(parApply(cl,tdata,1,myfun))		
		stopCluster(cl) #stop the cluster for analysis
		###need to store the outputs
		NumGCMs_d[,(grep(yr,colnames(NumGCMs_d)))]=tout[,]
	}

save(NumGCMs_d,file=paste(out.dir,es,"_",tax,'_Decreased_Rich_GCMs.Rdata',sep=''))

}




##############################################################################################################################################
### Creating images

source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix); library(maptools)
Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile

#### Set basic parameters

base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
image.dir = '/home/jc246980/SDM/Richness/Images/'; setwd(image.dir)


#### Select options for future richness

taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] # select species
PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
es='RCP85'												# select RCP
yr=2085													# Select year (for projected future)

											
png(paste(es,'_Rich_agree_GCMs_',yr,'.png',sep=''),width=dim(base.asc)[1]*2+200, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	
par(mar=c(0,3,0,3),mfrow=c(4,2),cex=1,oma=c(10,10,10,0)) #define the plot parameters

for (tax in taxa) {print(tax)

	load(paste('/home/jc246980/SDM/Richness/',es,"_",tax,"_Increased_Rich_GCMs.Rdata",sep=''))
	load(paste('/home/jc246980/SDM/Richness/',es,"_",tax,"_Decreased_Rich_GCMs.Rdata",sep=''))

	cols=colorRampPalette(c("#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(11)
	pos=tpos
	pos=merge(pos,NumGCMs, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste('yr_',yr,sep='')])
	zlim = range(c(1,as.vector(tasc)),na.rm=TRUE)	
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    if (tax==c("frog")) color.legend(118,-42,140,-41,zlim,cols,cex=8)

	cols=colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))(11)
	cols=cols[11:1]
	pos=tpos
	pos=merge(pos,NumGCMs_d, by='SegmentNo',all.x=TRUE)
	tasc=make.asc(pos[,paste('yr_',yr,sep='')])
	zlim = range(c(1,as.vector(tasc)),na.rm=TRUE)	
	image(base.asc,ann=F,axes=F,col='white')
	image(tasc,ann=F,axes=F,col=cols,zlim=zlim, add=TRUE)
	plot(Drainageshape , lwd=10, ann=FALSE,axes=FALSE, add=TRUE)
    if (tax==c("frog")) color.legend(118,-42,140,-41,zlim,cols,cex=8)

}

 mtext(c("Frogs", "Turtles", "Crayfish", "Fish"),side=2,line=1,outer=TRUE,cex=12,at=seq(1/8,0.99,1/4))
 mtext(c('Increase','Decrease'),side=3,line=1,outer=TRUE,cex=12,at=seq(1/4,0.99,1/2))
 
 dev.off ()
