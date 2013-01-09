#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################
library(SDMTools) #load the necessary library

wd = '~/working/NARP_stability/OZ_5km/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('data/base.asc.gz') #read in the base ascii grid file
pos = read.csv('data/base.positions.csv',as.is=TRUE) #read in the base positions file

vois = c('pre','tmx','tmp','tmn') #define the variables
ESs = unlist(strsplit(list.files(paste('data/monthly/',vois[1],sep='')),'_')); ESs = unique(ESs[seq(1,length(ESs),2)]); ESs = ESs[-which(ESs=="1990")] #list the emission scenarios
GCMs = list.files(paste('data/monthly/',vois[1],sep=''),pattern=ESs[1]); GCMs = gsub(paste(ESs[1],'_',sep=''),'',GCMs); GCMs = gsub('.Rdata','',GCMs) #get a list of GCMs
YEARs = seq(2015,2085,10) #define the years

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
legend.pnts = cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38)) #define the location of the legend

###prepare the data
for (voi in vois) { cat(voi,'\n') #cycle through each variable of interest
	load(paste('data/annual/',voi,'.Rdata',sep='')) #load the data
	annualdelta = annualpercent = annualsd = annualdata #copy annualdata to be replaced with number of standard deviation and percentiles
	for (ii in 3:ncol(annualdata)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
		annualpercent[,ii] = pnorm(annualdata[,ii],mean=annualdata[,1],sd=annualdata[,2]) #calculate the percentile given a mean and sd
		annualsd[,ii] = (annualdata[,ii]-annualdata[,1])/annualdata[,2] #calculate the number of standard deviations away
	}; cat('\n')
	if (voi=='pre') { 
		annualdelta[,-c(1:2)] = annualdelta[,-c(1:2)] / annualdelta[,1] #convert to deltas as a proportion
		# use to define limits.... range(annualdelta[,-c(1:2)]); quantile(annualdelta[,-c(1:2)],c(0.01,0.5,0.99))
		deltalims = c(0.6,1.4); #define the delta limits
		sdlims = c(-1.1,1.1) #define the sd limits ... range(annualsd[,-c(1:2)]); quantile(annualsd[,-c(1:2)],c(0.01,0.5,0.99))
		percentlims = c(0,1) #define the percentile limits 		
		cols = all.cols #define the plot colors
	} else {
		annualdelta[,-c(1:2)] = annualdelta[,-c(1:2)] - annualdelta[,1] #convert to deltas as an absolute difference
		# use to define limits.... range(annualdelta[,-c(1:2)]); quantile(annualdelta[,-c(1:2)],c(0.01,0.5,0.99))
		deltalims = c(0,5); #define the delta limits
		sdlims = c(0,7) #define the sd limits ... range(annualsd[,-c(1:2)]); quantile(annualsd[,-c(1:2)],c(0.01,0.5,0.99))
		percentlims = c(0,1) #define the percentile limits 
		cols = all.cols[21:1]
	}
	
	###create the individual images
	dir.create(paste('images/annual/',voi,'/individuals/',sep=''),recursive=TRUE)
	for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse
		for (gcm in GCMs) { cat(gcm,'\n') #cycle through each GCM & prepare all data for reuse
			for (year in YEARs) { cat(year,'\n') #cycle through each of the years
				png(paste('images/annual/',voi,'/individuals/',es,'_',gcm,'_',year,'.png',sep=''),width=dim(base.asc)[1]*3, height=dim(base.asc)[2]*1+25, units='px', pointsize=20, bg='lightgrey')
					par(mar=c(0,0,0,0),mfrow=c(1,3),cex=1,oma=c(0,0,3,0)) #define the plot parameters
					##plot the delta
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = annualdelta[,paste(es,gcm,year,sep='_')] #get the data
					tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
					legend.gradient(legend.pnts,cols=cols,cex=1,title='delta',limits=c(paste('<',deltalims[1]),paste('>',deltalims[2]))) #add the legend gradient
					##plot the sd
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = annualsd[,paste(es,gcm,year,sep='_')] #get the data
					tasc[which(tasc<sdlims[1])] = sdlims[1]; tasc[which(tasc>sdlims[2])] = sdlims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols) #create the image
					legend.gradient(legend.pnts,cols=cols,cex=1,title='num sd from current',limits=c(paste('<',sdlims[1]),paste('>',sdlims[2]))) #add the legend gradient
					##plot the percentile
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = annualdelta[,paste(es,gcm,year,sep='_')] #get the data
					tasc[which(tasc<percentlims[1])] = percentlims[1]; tasc[which(tasc>percentlims[2])] = percentlims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=percentlims,col=cols) #create the image
					legend.gradient(legend.pnts,cols=cols,cex=1,title='percentile',limits=percentlims) #add the legend gradient
					
					mtext(paste(es,gcm,year,sep=' - '),line=1,outer=TRUE,cex=2)
				dev.off() #close out the image
			}
		}
	}
}

for (voi in vois) { cat(voi,'\n') #cycle through each variable of interest
	load(paste('data/annual/',voi,'.Rdata',sep='')) #load the data
	annualdelta = annualpercent = annualsd = annualdata #copy annualdata to be replaced with number of standard deviation and percentiles
	for (ii in 3:ncol(annualdata)) { cat(ii,'...') #cycle through each of the es, gcm and year combinations
		annualsd[,ii] = (annualdata[,ii]-annualdata[,1])/annualdata[,2] #calculate the number of standard deviations away
	}; cat('\n')
	if (voi=='pre') { 
		annualdelta[,-c(1:2)] = annualdelta[,-c(1:2)] / annualdelta[,1] #convert to deltas as a proportion
	} else {
		annualdelta[,-c(1:2)] = annualdelta[,-c(1:2)] - annualdelta[,1] #convert to deltas as an absolute difference
	}
	
	###now create the summary images
	outdelta = matrix(NA,nrow=nrow(annualdata),ncol=3*length(ESs)*length(YEARs)); #define the output matrix
	tt = expand.grid(c(10,50,90),YEARs,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt #add the column names
	outsd = outdelta #copy the outdelta
	for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse
		for (year in YEARs) { cat(year,'\n') #cycle through each of the years 
			cois = intersect(grep(year,colnames(annualdelta)),grep(es,colnames(annualdelta))) #define the columns that intersect the ES & year
			outquant = t(apply(annualdelta[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
			outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
			outquant = t(apply(annualsd[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
			outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data
		}
	}
	dir.create(paste('images/annual/',voi,'/summary/',sep=''),recursive=TRUE)
	save(outdelta,outsd,file=paste('images/annual/',voi,'/summary/percentiles.Rdata',sep=''))
}

library(plotrix)
for (voi in vois) { cat(voi,'\n') #cycle through each variable of interest
	load(paste('data/annual/',voi,'.Rdata',sep='')) #load the data
	load(paste('images/annual/',voi,'/summary/percentiles.Rdata',sep='')) #load the data
	if (voi=='pre') {
		outdelta[,] = outdelta[,] / annualdata[,1] #create the percentile delta
		deltalims = c(0.6,1.4); #define the delta limits
		deltalabs = c(paste('<',deltalims[1]),1,paste('>',deltalims[2]))
		sdlims = c(-1.1,1.1) #define the sd limits ... range(annualsd[,-c(1:2)]); quantile(annualsd[,-c(1:2)],c(0.01,0.5,0.99))
		sdlabs = c(paste('<',sdlims[1]),0,paste('>',sdlims[2]))
		cols = all.cols #define the plot colors
	} else {
		outdelta[,] = outdelta[,] - annualdata[,1] #create the percentile delta
		deltalims = c(0,5); #define the delta limits
		deltalabs = c(paste('<',deltalims[1]),2.5,paste('>',deltalims[2]))
		sdlims = c(0,7) #define the sd limits ... range(annualsd[,-c(1:2)]); quantile(annualsd[,-c(1:2)],c(0.01,0.5,0.99))
		sdlabs = c(paste('<',sdlims[1]),3.5,paste('>',sdlims[2]))
		cols = all.cols[21:1]
	}
	
	for (year in YEARs) { cat(year,'\n') #cycle through each of the years
		##first work with RCPs
		ESoi = ESs[grep('RCP',ESs)] #define the emission scenarios of interest
		png(paste('images/annual/',voi,'/summary/RCP_delta_',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
			par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
			first=TRUE
			for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
				for (es in ESoi) { cat(es,'\n') #cycle through the emission scenarios of interst
					##plot the delta
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outdelta[,paste(es,year,percentile,sep='_')] #get the data
					tasc[which(tasc<deltalims[1])] = deltalims[1]; tasc[which(tasc>deltalims[2])] = deltalims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=deltalims,col=cols) #create the image
					if (percentile==90 & es==ESoi[length(ESoi)]) color.legend(118,-44,140,-41,deltalabs,cols,cex=2)
				}
			}
			mtext(ESoi,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
			if (voi =='pre') {
				mtext(paste(year,'Proportionate Change in Annual Precipitation'),side=1,line=1,outer=TRUE,cex=3)
			} else {
				mtext(paste(year,'Change in Annual Temperature'),side=1,line=1,outer=TRUE,cex=3)
			}
			mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
		dev.off() #close out the image
		png(paste('images/annual/',voi,'/summary/RCP_sd_',year,'.png',sep=''),width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=20, bg='lightgrey')
			par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1,oma=c(3,3,3,0)) #define the plot parameters
			for (percentile in c(10,50,90)) { cat(percentile,'\n') #cycle through the percentiles
				for (es in ESoi) { cat(es,'\n') #cycle through the emission scenarios of interst
					##plot the delta
					tasc = base.asc; tasc[cbind(pos$row,pos$col)] = outsd[,paste(es,year,percentile,sep='_')] #get the data
					tasc[which(tasc<sdlims[1])] = sdlims[1]; tasc[which(tasc>sdlims[2])] = sdlims[2] #ensure all data within limits
					image(tasc,ann=FALSE,axes=FALSE,zlim=sdlims,col=cols) #create the image
					if (percentile==90 & es==ESoi[length(ESoi)]) color.legend(118,-44,140,-41,sdlabs,cols,cex=2)
				}
			}
			mtext(ESoi,side=3,line=1,outer=TRUE,cex=2,at=seq(1/8,0.99,1/4))
			if (voi =='pre') {
				mtext(paste(year,' -- Number of Standard Deviations from Current Precipitation Norm'),side=1,line=1,outer=TRUE,cex=3)
			} else {
				mtext(paste(year,' -- Number of Standard Deviations from Current Temperature Norm'),side=1,line=1,outer=TRUE,cex=3)
			}
			mtext(c('90th','50th','10th'),side=2,line=1,outer=TRUE,cex=2,at=seq(1/6,0.99,1/3))
		dev.off() #close out the image
	

		
	}
		
	
}

par(mar=c(7,4,4,6))
 testcol<-color.gradient(c(0,1),0,c(1,0),nslices=5)
 col.labels<-c("Cold","Warm","Hot")
 color2D.matplot(matrix(rnorm(100),nrow=10),c(1,0),0,c(0,1),
  main="Test color legends")
 color.legend(11,6,11.8,9,col.labels,testcol,gradient="y")
 color.legend(10.2,2,11,5,col.labels,testcol,align="rb",gradient="y")
 color.legend(0.5,-2,3.5,-1.2,col.labels,testcol)
 color.legend(7,-1.8,10,-1,col.labels,testcol,align="rb",col=testcol[c(1,3,5)])
 par(mar=c(5,4,4,2))









futdir = '~/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it

#prepare the current climates and extract mean/sd
file.copy('~/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz','data/base.asc.gz') #copy the base asc
file.copy('~/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv','base.positions.csv') #copy the base positions
curfiles = paste('~/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/',
	c('rain19402009.csv','tmax19402009.csv','tmean19402009.csv','tmin19402009.csv'),sep=''); names(curfiles) = vois #define the current files



curdata = read.csv(paste(awapdir,curfiles[ii],sep=''),as.is=TRUE); curdata = as.matrix(curdata) #read in teh current monthly data
	cois = c(1,2); for (yy in 1976:2005) {cois = c(cois,grep(yy,colnames(curdata)))} #get the columns in the years of interest
	curdata = curdata[,cois] #subset the current data set
	curmean = matrix(NA,nrow=nrow(curdata),ncol=12); colnames(curmean) = paste(vois[ii],sprintf('%02i',1:12),sep=''); cursd=curmean #define some output data matrices to store mean and standard deviation
	for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months
		curmean[,mm] = rowMeans(curdata[,which(as.numeric(substr(colnames(curdata),7,8))==mm)],na.rm=TRUE) #calculate row means
		cursd[,mm] = apply(curdata[,which(as.numeric(substr(colnames(curdata),7,8))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
	}
	


library(SDMTools) #load the necessary library

wd = '~/working/NARP_stability/OZ_5km/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('data/base.asc.gz') #read in the base ascii grid file
pos = read.csv('data/base.positions.csv',as.is=TRUE) #read in the base positions file

load(paste('data/',voi,'/1990_mean.Rdata',sep='')) #load in current means
load(paste('data/',voi,'/1990_sd.Rdata',sep='')) #load in current sd
cursd[which(cursd==0)] = 0.00000000000001 #ensure no 0's

#read in all the future
GCMs = list.files(paste('data/',voi,sep=''),pattern=es); GCMs = gsub(paste(es,'_',sep=''),'',GCMs); GCMs = gsub('.Rdata','',GCMs) #get a list of GCMs
for (gcm in GCMs) { cat(gcm,'\n') #cycle through each GCM & prepare all data for reuse
	load(paste('data/',voi,'/',es,'_',gcm,'.Rdata',sep='')) #load in current sd

	futpercent = futsd = futdata #copy futdata to be replaced with outputs
	for (ii in 1:ncol(futdata)) { cat(ii,'...') #cycle through each of the years
		mm = as.numeric(substr(colnames(futdata),8,9))[ii] #define the month for current
		futpercent[,ii] = pnorm(futdata[,ii],mean=curmean[,mm],sd=cursd[,mm]) #calculate the percentile given a mean and sd
		futsd[,ii] = (futdata[,ii]-curmean[,mm])/cursd[,mm] #calculate the number of standard deviations away
	}; cat('\n')
	#futpercent = round(futpercent,3); futsd = round(futsd,1)

	###create some images
	all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)#colorRampPalette(c('red4','orangered','gold','beige','tan','lightblue4','blue4'))(21) #define a set of colors
	legend.pnts = cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38)) #define the location of the legend
	
	##map the current standard deviation
	if (!file.exists(paste('images/',voi,'/1990_sd.png',sep=''))) { #only create current image if it does not exist
		dir.create(paste('images/',voi,sep=''),recursive=TRUE) #create the directory
		zlimits = c(0,quantile(as.vector(cursd[,]),probs=0.99)) #get the zlimits as 0 to 99 percentile
		cols = all.cols[11:1]
		png(paste('images/',voi,'/1990_sd.png',sep=''),width=dim(base.asc)[1]*4, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='lightgrey')
			par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1) #define the plot parameters
			for (mm in 1:12) { cat(mm,'. ') #cycle through months of interest
				tasc = base.asc; tasc[cbind(pos$row,pos$col)] = cursd[,mm] #get the data
				tasc[which(tasc>zlimits[2])] = zlimits[2] #convert anything over upper limit to upper limit
				image(tasc,ann=FALSE,axes=FALSE,zlim=zlimits,col=cols) #create the image
				legend('topleft',legend=sprintf('%02i',mm),bty='n',cex=2) #add the month in the corner
				if (mm==1) legend.gradient(legend.pnts,cols=cols,cex=2,title='current sd',limits=c(round(zlimits[1],2),paste('>',round(zlimits[2],2),sep=''))) #add the legend gradient
			}; cat('\n')		
		dev.off() #close the image		
	}
	
	##map the future number of standard deviations away
	if (voi=='pre') { zlimits = c(-1.5,1.5); cols = all.cols } else { zlimits = c(0,5); cols = all.cols[21:1] }#range(c(0,quantile(na.omit(as.vector(futsd)),probs=c(0.01,0.99)))) #get the zlimits as 0 to 99 percentile
	png(paste('images/',voi,'/',es,'_',gcm,'_sd.png',sep=''),width=dim(base.asc)[1]*4, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='lightgrey')
		par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1) #define the plot parameters
		for (mm in 1:12) { cat(mm,'. ') #cycle through months of interest
			tasc = base.asc; tasc[cbind(pos$row,pos$col)] = futsd[,paste(voi,2085,sprintf('%02i',mm),sep='')] #get the data
			tasc[which(tasc>zlimits[2])] = zlimits[2] #convert anything over upper limit to upper limit
			tasc[which(tasc<zlimits[1])] = zlimits[1] #convert anything over upper limit to upper limit
			image(tasc,ann=FALSE,axes=FALSE,zlim=zlimits,col=cols) #create the image
			legend('topleft',legend=sprintf('%02i',mm),bty='n',cex=2) #add the month in the corner
			if (mm==1) {
				if (zlimits[1]<0) {
					zlim = c(paste('<',round(zlimits[1],2),sep=''),paste('>',round(zlimits[2],2),sep=''))
				} else { zlim = c(round(zlimits[1],2),paste('>',round(zlimits[2],2),sep='')) }
				legend.gradient(legend.pnts,cols=cols,cex=2,title='sd',limits=zlim)	#add the legend gradient
			}
		}; cat('\n')		
	dev.off() #close the image		
	
	##map the future percentile
	zlimits = c(0,1) #get the zlimits as 0 to 99 percentile
	if (voi=='pre') { cols = all.cols } else { cols = all.cols[21:1] }#range(c(0,quantile(na.omit(as.vector(futsd)),probs=c(0.01,0.99)))) #get the zlimits as 0 to 99 percentile
	png(paste('images/',voi,'/',es,'_',gcm,'_percentile.png',sep=''),width=dim(base.asc)[1]*4, height=dim(base.asc)[2]*3, units='px', pointsize=20, bg='lightgrey')
		par(mar=c(0,0,0,0),mfrow=c(3,4),cex=1) #define the plot parameters
		for (mm in 1:12) { cat(mm,'. ') #cycle through months of interest
			tasc = base.asc; tasc[cbind(pos$row,pos$col)] = futpercent[,paste(voi,2085,sprintf('%02i',mm),sep='')] #get the data
			image(tasc,ann=FALSE,axes=FALSE,zlim=zlimits,col=cols) #create the image
			legend('topleft',legend=sprintf('%02i',mm),bty='n',cex=2) #add the month in the corner
			if (mm==1) legend.gradient(legend.pnts,cols=cols,cex=2,title='percentile',limits=c(0,1)) #add the legend gradient
		}; cat('\n')		
	dev.off() #close the image		
}

