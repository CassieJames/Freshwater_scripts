################################################################################
# 5 km figures aggregated over 30 year record using my old script 'Hydrology_current_5km_working'

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

data.dir='/home/jc246980/Hydrology.trials/Output_1960_1990';setwd(data.dir)
load(paste(data.dir,'/Q_run_30yearagg_dynamic_1960.Rdata',sep=''))

yois=1960:1990
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun) = tt #add the column names

Q_run_curmean=NULL
Q_run_cursd=NULL


      for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],na.rm=TRUE) #calculate row means
           tdata_cursd = apply(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
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
png(paste(data.dir,'/Q_run_aggmean_1960_1990.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
	par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0)) #make 4 columns of 3 rows of images
	for (ii in 1:12) { cat(ii, '\n')
		Qrun.agg.asc=base.asc; Qrun.agg.asc[cbind(pos$row,pos$col)]=Q_run_curmean[,ii]
		image(Qrun.agg.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		text (130, -40, months[ii], cex=4)
		if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim,digits=4), title='Qrun means 1960-1990', cex=3)}
	}

dev.off()