
	source('/home/jc148322/scripts/libraries/cool_functions.r')
	library(SDMTools);library(plotrix); library(foreign); library(maptools);library(ggplot2)
	Drainageshape = readShapePoly('/home/jc246980/Janet_Stein_data/Drainage_division') #read in your shapefile


	base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
	pos=make.pos(base.asc)
	pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
	tpos=pos

	image.dir = '/home/jc246980/SDM/Paper_images/'; setwd(image.dir)
	terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')
	taxa=c('fish','crayfish','turtles','frog');tax=taxa[1] # select species
	PERC=c('10','50','90'); percentile=PERC[2]				# Select percentile to plot
	es='RCP85'												# select RCP
	yr=2085													# Select year (for projected future)


	
	png('current_elevation.png',width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*2+300, units='px', pointsize=20, bg='WHITE')	
	par(mar=c(0,3,0,3),mfrow=c(1,1),cex=10,oma=c(10,10,10,0), lwd=13) #define the plot parameters
	
	
	tax=c("fish")	
	col='blue'
	pos=tpos
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # load current
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	pos=merge(pos,terrain[,c('SEGMENTNO','STRELEMEAN', 'CATELEMAX' )], by.x='SegmentNo', by.y='SEGMENTNO', all.x=TRUE)
	pos=pos[!duplicated(pos$SegmentNo), ]
	pos$segelevsmax=pos$STRELEMEAN/pos$CATELEMAX
	
	posfish <- pos[order(pos$STRELEMEAN),] 

	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posfish) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, median)
	  data.frame(x = unique(x2), y= y2)
	}
	
	bin_plot_dat_ele <- function(bin_size){
	  nr_bins <- nrow(posfish) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$STRELEMEAN, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
	
	plot_dat2ele <- bin_plot_dat_ele(18)
	plot_dat2 <- bin_plot_dat(18)
	plot(plot_dat2ele$y,plot_dat2$y, type='n', xaxt="n",yaxt="n",ann=FALSE,ylim=c(0,30), xlim=c(0,2000))
	axis(2,col.axis="black",las=1, lwd.ticks=10)
	axis(1,col.axis="black",las=1, lwd.ticks=10)
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	lines(plot_dat2ele$y, predict(loess_fit), col = "blue", lwd=10)
	
	
	tax=c("crayfish")
	col= 'red'
	pos=tpos
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # load current
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	pos=merge(pos,terrain[,c('SEGMENTNO','STRELEMEAN', 'CATELEMAX' )], by.x='SegmentNo', by.y='SEGMENTNO', all.x=TRUE)
	pos=pos[!duplicated(pos$SegmentNo), ]
	pos$segelevsmax=pos$STRELEMEAN/pos$CATELEMAX
	
	poscray <- pos[order(pos$STRELEMEAN),] 

	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(poscray) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
		
	plot_dat2 <- bin_plot_dat(18)
	par(new=T) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y,axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4, ylim=c(0,15),col.axis="black",las=1, lwd.ticks=10)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	
	
	
	tax=c("turtles")	
	col= 'grey'
	pos=tpos
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # load current
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	pos=merge(pos,terrain[,c('SEGMENTNO','STRELEMEAN', 'CATELEMAX' )], by.x='SegmentNo', by.y='SEGMENTNO', all.x=TRUE)
	pos=pos[!duplicated(pos$SegmentNo), ]
	pos$segelevsmax=pos$STRELEMEAN/pos$CATELEMAX
	
	posturtle <- pos[order(pos$STRELEMEAN),] 

	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posturtle) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, median)
	  data.frame(x = unique(x2), y= y2)
	}
	
	plot_dat2 <- bin_plot_dat(18)
	par(new=TRUE) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y, axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4, xaxt="n",yaxt="n",ann=FALSE)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	
	
	tax=c("frog")
	col='green'
	pos=tpos
	load(paste('/home/jc246980/SDM/Richness/Clip4North/',tax,'/RCP3PD_Richness_current.mat.Rdata',sep='')) # load current
	pos=merge(pos,Richness_current, by='SegmentNo',all.x=TRUE)
	pos=merge(pos,terrain[,c('SEGMENTNO','STRELEMEAN', 'CATELEMAX' )], by.x='SegmentNo', by.y='SEGMENTNO', all.x=TRUE)
	pos=pos[!duplicated(pos$SegmentNo), ]
	pos$segelevsmax=pos$STRELEMEAN/pos$CATELEMAX
	
	posfrog <- pos[order(pos$STRELEMEAN),] 

	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posfrog) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
	
	
	plot_dat2 <- bin_plot_dat(18)
	par(new=T) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y, axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4,xaxt="n",yaxt="n",ann=FALSE)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	
	legend("topleft",col=c("blue","red", "grey", "green"),lty=0,legend=c("Fish","Crayfish", "Turtles", "Frogs"))
	
	dev.off()
	
	###################### Quicker plot if pos data is already in memory for all taxa
	
	png('current_elevation.png',width=dim(base.asc)[1]*2+300, height=dim(base.asc)[2]*1+300, units='px', pointsize=20, bg='WHITE')	
	par(mar=c(0,3,0,3),mfrow=c(1,1),cex=10,oma=c(5,5,5,0), lwd=13, xpd=TRUE) #define the plot parameters

	
	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posfish) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, median)
	  data.frame(x = unique(x2), y= y2)
	}
	
	bin_plot_dat_ele <- function(bin_size){
	  nr_bins <- nrow(posfish) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$STRELEMEAN, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
	
	plot_dat2ele <- bin_plot_dat_ele(18)
	plot_dat2 <- bin_plot_dat(18)
	plot(plot_dat2ele$y,plot_dat2$y, type='n', xaxt="n",yaxt="n",ann=FALSE,ylim=c(0,30), xlim=c(0,2000))
	axis(2,col.axis="black",las=1, lwd.ticks=10)
	axis(1,col.axis="black",las=1, lwd.ticks=10)
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	lines(plot_dat2ele$y, predict(loess_fit), col = "blue", lwd=10)
			
		
	 bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(poscray) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
		
	plot_dat2 <- bin_plot_dat(18)
	par(new=T) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y,axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4, ylim=c(0,15),col.axis="black",las=1, lwd.ticks=10)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	
	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posturtle) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, median)
	  data.frame(x = unique(x2), y= y2)
	}
	
	plot_dat2 <- bin_plot_dat(18)
	par(new=TRUE) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y, axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4, xaxt="n",yaxt="n",ann=FALSE)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	
		
	bin_plot_dat <- function(bin_size){
	  nr_bins <- nrow(posfrog) / bin_size
	  x2 <- rep(1:nr_bins * bin_size, each = bin_size)
	  y2 <- tapply(pos$Current, x2, max)
	  data.frame(x = unique(x2), y= y2)
	}
	
	
	plot_dat2 <- bin_plot_dat(18)
	par(new=T) # put data other than fish on second y axis to different scale
	plot(plot_dat2ele$y,plot_dat2$y, axes=F, type='n')
	loess_fit <- loess(plot_dat2$y ~ plot_dat2$x, na.action = na.exclude)
	axis(4,xaxt="n",yaxt="n",ann=FALSE)
	lines(plot_dat2ele$y, predict(loess_fit), col = col, lwd=10)
	text(10,28,"(c)",cex=14)
	
	legend("topright",col=c("blue","red", "grey", "green"),lty=0,legend=c("Fish","Crayfish", "Turtles", "Frogs"), inset=c(-0.2,0))
		
					  
	dev.off()	

	
	