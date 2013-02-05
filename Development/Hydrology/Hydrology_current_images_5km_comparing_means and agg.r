################################################################################
# 5 km figures aggregated over 30 year record using my old script 'Hydrology_current_5km_working'

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

data.dir='/home/jc246980/Hydrology.trials/Output_1976_2005';setwd(data.dir)
load(paste(data.dir,'/Q_run_30yearagg.Rdata',sep=''))

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Q_run) = tt #add the column names

Q_run_curmean=NULL
Q_run_cursd=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           Q_run_curmean=cbind(Q_run_curmean,tdata_curmean)
           Q_run_cursd=cbind(Q_run_cursd,tdata_cursd)
      }

tt = sprintf('%02i',1:12)
colnames(Q_run_curmean)= tt

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Q_run_curmean,na.rm=T),max(Q_run_curmean,na.rm=T))
png(paste(data.dir,'/Q_run_aggmean_1976_2005.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.agg.asc=base.asc; Qrun.agg.asc[cbind(pos$row,pos$col)]=Q_run_curmean[,ii]
		image(Qrun.agg.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun_agg_means_1976_2005', cex=3)}
	}

dev.off()

################################################################################
# Plot showing hydrology output based on thirty year means for 1976-2005


load(paste(data.dir,'/Qrun.current_5km_means.Rdata',sep=''))


months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Qrun,na.rm=T),max(Qrun,na.rm=T))
png(paste(data.dir,'/Qrun_means_1976_2005.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun[,ii]
		image(Qrun.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun means 1976_2005', cex=3)}
	}

dev.off()

################################################################################
# Plot showing dynamic budyko output for aggregated means over 30 years using dynamic Budyko

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

data.dir='/home/jc246980/Hydrology.trials/Output_1976_2005';setwd(data.dir)
load(paste(data.dir,'/Q_run_30yearagg_dynamic.Rdata',sep=''))

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun) = tt #add the column names

Q_run_curmean_dynamo=NULL
Q_run_cursd_dynamo=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           Q_run_curmean_dynamo=cbind(Q_run_curmean_dynamo,tdata_curmean)
           Q_run_cursd_dynamo=cbind(Q_run_cursd_dynamo,tdata_cursd)
      }

tt = sprintf('%02i',1:12)
colnames(Q_run_curmean_dynamo)= tt

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Q_run_curmean_dynamo,na.rm=T),max(Q_run_curmean_dynamo,na.rm=T))
png(paste(data.dir,'/Q_run_aggmean_1976_2005_dynamic.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.agg.asc=base.asc; Qrun.agg.asc[cbind(pos$row,pos$col)]=Q_run_curmean_dynamo[,ii]
		image(Qrun.agg.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun_agg_means_1976_2005_dynamic', cex=3)}
	}

dev.off()    


################################################################################
# Plot showing differences between agg and means hydrology for current

Q_run_curmean_1= Q_run_curmean+1
Qrun_1=Qrun+1

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
#zlim=c(min(Qrun,na.rm=T),max(Qrun,na.rm=T))
zlimmax=quantile((Q_run_curmean_1/Qrun_1),c(0.01,0.99))
zlim=c(0,35.9081600)
#zlim=c(min(Q_run_curmean_1/Qrun_1),max(Q_run_curmean_1/Qrun_1))

png(paste(data.dir,'/Qrun_proportional_diffs.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Q_run_curmean_1[,ii]/Qrun_1[,ii]
		image(Qrun.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun agg means / Qrun  means', cex=3)}
	}

dev.off()

################################################################################
# Annuals

Qrun_annuals = apply(Qrun,1,sum)
Q_run_curmean_annuals = apply(Q_run_curmean,1,sum)
Q_run_curmean_dynamo_annuals= apply(Q_run_curmean_dynamo,1,sum)
pnts=cbind(x=c(112,142,142,116), y=c(-44,-42,-44,-42))#define the location of the legend
cols = colorRampPalette(c('brown3','yellow','forestgreen','deepskyblue3','slateblue'))(11)


png(paste(data.dir,'/Annuals_teng_cats.png',sep=''),width=dim(base.asc)[1]*2/100, height=dim(base.asc)[2]*1/100, units='cm', res=300, pointsize=5, bg='white')
  par(mar=c(0,0,0,0),mfrow=c(1,3),cex=1,oma=c(0,0,4,0))
    Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun_annuals
    Qrun.asc[which(Qrun.asc==0)] = 0
  	Qrun.asc[which(Qrun.asc>0 & Qrun.asc<5)] = 1
	  Qrun.asc[which(Qrun.asc>=5 & Qrun.asc<10)] = 2
	  Qrun.asc[which(Qrun.asc>=10 & Qrun.asc<25)] = 3
	  Qrun.asc[which(Qrun.asc>=25 & Qrun.asc<50)] = 4
  	Qrun.asc[which(Qrun.asc>=50 & Qrun.asc<75)] = 5
	  Qrun.asc[which(Qrun.asc>=75 & Qrun.asc<100)] = 6
	  Qrun.asc[which(Qrun.asc>=100 & Qrun.asc<200)] = 7
    Qrun.asc[which(Qrun.asc>=200 & Qrun.asc<300)] = 8
  	Qrun.asc[which(Qrun.asc>=300 & Qrun.asc<500)] = 9
	  Qrun.asc[which(Qrun.asc>=500 & Qrun.asc<1000)] = 10
	  Qrun.asc[which(Qrun.asc>=1000 & Qrun.asc<3500)] = 11
    Qrun.asc[which(Qrun.asc>=3500)] = 12
	  zlims = range(c(0,as.vector(Qrun.asc)),na.rm=TRUE)
  	image(Qrun.asc, ann=FALSE,axes=FALSE,col=cols,zlim=zlims)
    legend.gradient(pnts,cols=cols,limits=round(zlims,digits=4), title='annual runnoff on 30 year means', cex=1) 
          
    Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Q_run_curmean_annuals
    Qrun.asc[which(Qrun.asc==0)] = 0
  	Qrun.asc[which(Qrun.asc>0 & Qrun.asc<5)] = 1
	  Qrun.asc[which(Qrun.asc>=5 & Qrun.asc<10)] = 2
	  Qrun.asc[which(Qrun.asc>=10 & Qrun.asc<25)] = 3
	  Qrun.asc[which(Qrun.asc>=25 & Qrun.asc<50)] = 4
  	Qrun.asc[which(Qrun.asc>=50 & Qrun.asc<75)] = 5
	  Qrun.asc[which(Qrun.asc>=75 & Qrun.asc<100)] = 6
	  Qrun.asc[which(Qrun.asc>=100 & Qrun.asc<200)] = 7
    Qrun.asc[which(Qrun.asc>=200 & Qrun.asc<300)] = 8
  	Qrun.asc[which(Qrun.asc>=300 & Qrun.asc<500)] = 9
	  Qrun.asc[which(Qrun.asc>=500 & Qrun.asc<1000)] = 10
	  Qrun.asc[which(Qrun.asc>=1000 & Qrun.asc<3500)] = 11
    Qrun.asc[which(Qrun.asc>=3500)] = 12
	  zlims = range(c(0,as.vector(Qrun.asc)),na.rm=TRUE)
  	image(Qrun.asc, ann=FALSE,axes=FALSE,col=cols,zlim=zlims)
    legend.gradient(pnts,cols=cols,limits=round(zlims,digits=4), title='annual runnoff for each year static Budyko', cex=1) 
    
    Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Q_run_curmean_dynamo_annuals
    Qrun.asc[which(Qrun.asc==0)] = 0
  	Qrun.asc[which(Qrun.asc>0 & Qrun.asc<5)] = 1
	  Qrun.asc[which(Qrun.asc>=5 & Qrun.asc<10)] = 2
	  Qrun.asc[which(Qrun.asc>=10 & Qrun.asc<25)] = 3
	  Qrun.asc[which(Qrun.asc>=25 & Qrun.asc<50)] = 4
  	Qrun.asc[which(Qrun.asc>=50 & Qrun.asc<75)] = 5
	  Qrun.asc[which(Qrun.asc>=75 & Qrun.asc<100)] = 6
	  Qrun.asc[which(Qrun.asc>=100 & Qrun.asc<200)] = 7
    Qrun.asc[which(Qrun.asc>=200 & Qrun.asc<300)] = 8
  	Qrun.asc[which(Qrun.asc>=300 & Qrun.asc<500)] = 9
	  Qrun.asc[which(Qrun.asc>=500 & Qrun.asc<1000)] = 10
	  Qrun.asc[which(Qrun.asc>=1000 & Qrun.asc<3500)] = 11
    Qrun.asc[which(Qrun.asc>=3500)] = 12
	  zlims = range(c(0,as.vector(Qrun.asc)),na.rm=TRUE)
  	image(Qrun.asc, ann=FALSE,axes=FALSE,col=cols,zlim=zlims)
    legend.gradient(pnts,cols=cols,limits=round(zlims,digits=4), title='annual runnoff for each year dynamic Budyko', cex=1) 
    

dev.off()  

################################################################################
# Plot showing static budyko output for aggregated means over 30 years

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

data.dir='/home/jc246980/Hydrology.trials/Output_1976_2005';setwd(data.dir)
load(paste(data.dir,'/Q_run_30yearagg.Rdata',sep=''))

yois=1976:2005
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Q_run) = tt #add the column names

Q_run_curmean_dynamo=NULL
Q_run_cursd_dynamo=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           Q_run_curmean_dynamo=cbind(Q_run_curmean,tdata_curmean)
           Q_run_cursd_dynamo=cbind(Q_run_cursd,tdata_cursd)
      }

tt = sprintf('%02i',1:12)
colnames(Q_run_curmean)= tt

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Q_run_curmean_dynamo,na.rm=T),max(Q_run_curmean_dynamo,na.rm=T))
png(paste(data.dir,'/Q_run_aggmean_1976_2005.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.agg.asc=base.asc; Qrun.agg.asc[cbind(pos$row,pos$col)]=Q_run_curmean_dynamo[,ii]
		image(Qrun.agg.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun_agg_means_1976_2005_dynamic', cex=3)}
	}

dev.off()        