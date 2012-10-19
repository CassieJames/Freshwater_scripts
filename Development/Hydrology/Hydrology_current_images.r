# setup plot info

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE)
wd='/home/jc246980/Hydrology.trials/Output_1976_2005_1km';setwd(wd)

################################################################################
#Figures for 1 km runoff using 5km Pawhc extracted to 1km
load(paste(wd,'/Qrun.current_1km.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Qrun,na.rm=T),max(Qrun,na.rm=T))
png(paste(wd,'/Q_run_current_1976_2005_1km_5kmPAWWHC.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun[,ii]
		image(Qrun.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun', cex=3)}
	}

dev.off()

load(paste(wd,'/Epot.current_1km.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Epot,na.rm=T),max(Epot,na.rm=T))
png(paste(wd,'/E_pot_current_1976_2005_1km_5kmPAWHC.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Epot.asc=base.asc; Epot.asc[cbind(pos$row,pos$col)]=Epot[,ii]
		image(Epot.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Epot', cex=3)}
	}

dev.off()

load(paste(wd,'/Eact.current_1km.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Eact,na.rm=T),max(Eact,na.rm=T))
png(paste(wd,'/E_act_current_1976_2005_1km_5kmPAWHC.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Eact.asc=base.asc; Eact.asc[cbind(pos$row,pos$col)]=Eact[,ii]
		image(Eact.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Eact', cex=3)}
	}

dev.off()

load(paste(wd,'/Rnet.current_1km.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Rnet,na.rm=T),max(Rnet,na.rm=T))
png(paste(wd,'/R_net_current_1976_2005_1km_5kmPAWHC.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Rnet.asc=base.asc; Rnet.asc[cbind(pos$row,pos$col)]=Rnet[,ii]
		image(Rnet.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Rnet', cex=3)}
	}

dev.off()

###############################################################################
#Figures for 1 km runoff using solpawhc

load(paste(wd,'/Qrun.current_1km_solpawhc.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Qrun,na.rm=T),max(Qrun,na.rm=T))
png(paste(wd,'/Q_run_current_1976_2005_1km_solpawhc.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.asc=base.asc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun[,ii]
		image(Qrun.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun', cex=3)}
	}

dev.off()

load(paste(wd,'/Epot.current_1km_solpawhc.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Epot,na.rm=T),max(Epot,na.rm=T))
png(paste(wd,'/E_pot_current_1976_2005_1km__solpawhc.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Epot.asc=base.asc; Epot.asc[cbind(pos$row,pos$col)]=Epot[,ii]
		image(Epot.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Epot', cex=3)}
	}

dev.off()

load(paste(wd,'/Eact.current_1km_solpawhc.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Eact,na.rm=T),max(Eact,na.rm=T))
png(paste(wd,'/E_act_current_1976_2005_1km_solpawhc.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Eact.asc=base.asc; Eact.asc[cbind(pos$row,pos$col)]=Eact[,ii]
		image(Eact.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Eact', cex=3)}
	}

dev.off()

load(paste(wd,'/Rnet.current_1km_solpawhc.Rdata',sep=''))

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
#image for runoff
zlim=c(min(Rnet,na.rm=T),max(Rnet,na.rm=T))
png(paste(wd,'/R_net_current_1976_2005_1km_solpawhc.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Rnet.asc=base.asc; Rnet.asc[cbind(pos$row,pos$col)]=Rnet[,ii]
		image(Rnet.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Rnet', cex=3)}
	}

dev.off()




















################################################################################
# 5 km figures aggregated over 30 year record
base.asc='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.asc.gz'
pos='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.positions.csv'