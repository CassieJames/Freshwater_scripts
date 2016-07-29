	
library(SDMTools)#load the necessary libraries
library(parallel)
source('/home/jc148322/scripts/libraries/cool_functions.r')
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[1]
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]

out.dir=paste("/home/jc246980/SDM/TurnOver/",tax,"/",sep="")
load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
YEARs=seq(2015,2085,10)

data.dir=paste('/home/jc246980/SDM/Invaders_contractors/',tax,'/Data/',sep='')
load(paste(data.dir,es,'.Invaders.mat.Rdata',sep=''))
load(paste(data.dir,es,'.Contractors.mat.Rdata',sep=''))

Turnover=((Invaders+Contractors))+1/(Richness_current[,2]+1)

#script to remove situations where current is zero and invaders are zero
out=NULL
for (i in 1:ncol (Turnover)) {cat(i,'\n')
tdata=cbind(Richness_current, Turnover[,i])
colnames(tdata)[3]="Future"
tdata$Future[tdata$Current==0 & tdata$Future==1]<-NA
if(i==1) {out=tdata} else {out=cbind(out, tdata[,3])}
}
colnames(out)=c("SegmentNo", "Current", colnames(Turnover))
Turnover=out
save(Turnover,file=paste(out.dir,es,'.Turnover.mat.Rdata',sep=''))

#load(paste(out.dir,es,'.Turnover.mat.Rdata',sep=''))
outquant_Turnover=NULL
for (yr in YEARs) { cat(yr,'\n')	
cois=grep(yr,colnames(Turnover))
tdata=Turnover[,cois]
tout = t(apply(tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
outquant_Turnover=cbind(outquant_Turnover,tout)
}

#### Alternative calculation of species turnover

library(SDMTools)#load the necessary libraries
library(parallel)
source('/home/jc148322/scripts/libraries/cool_functions.r')
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[1]
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]

out.dir=paste("/home/jc246980/SDM/TurnOver/",tax,"/",sep="")
load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # Current
YEARs=seq(2015,2085,10)

data.dir=paste('/home/jc246980/SDM/Invaders_contractors/',tax,'/Data/',sep='')
load(paste(data.dir,es,'.Invaders.mat.Rdata',sep=''))
load(paste(data.dir,es,'.Contractors.mat.Rdata',sep=''))

Turnover=(Invaders+Contractors)/(Richness_current[,2]+Invaders)

head(tdata[which(tdata$Current==0 & tdata$Future==0),])

#tdata=cbind(Richness_current$SegmentNo, Turnover)
save(Turnover,file=paste(out.dir,es,'.Turnover.mat_plus_gains.Rdata',sep=''))

### calculate quantiles
taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[1]
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]
YEARs=seq(2015,2085,10)
out.dir=paste("/home/jc246980/SDM/TurnOver/",tax,"/",sep="")

load(paste(out.dir,es,'.Turnover.mat_plus_gains.Rdata',sep=''))

is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

outquant_Turnover=NULL
for (yr in YEARs) { cat(yr,'\n')	
cois=grep(yr,colnames(Turnover))
tdata=Turnover[,cois]
#tdata[is.nan(tdata)] <- 0
tout = t(apply(tdata,1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
outquant_Turnover=cbind(outquant_Turnover,tout)
}

out.dir=paste("/home/jc246980/SDM/TurnOver/",sep="")
save(outquant_Turnover,file=paste(out.dir,es,"_",tax,"_Turnover_quants_gains.Rdata",sep=''))

