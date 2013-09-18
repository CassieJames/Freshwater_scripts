source('/home/jc148322/scripts/libraries/cool_functions.r')
library(SDMTools);library(plotrix)

es='RCP85'
yr=2085
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos=make.pos(base.asc)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc)
tpos=pos
image.dir = '/home/jc246980/SDM/Invaders_contractors/Images/'
taxa=c('fish','crayfish','turtles','frog')

for (tax in taxa) {
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Invaders.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Contractors.Rdata',sep=''))
		load(paste('/home/jc246980/SDM/Invaders_contractors/',es,"_",tax,'_Retainers.Rdata',sep=''))
		
		zlim_I=c(0,round(max(outquant_Invaders[,2:ncol(outquant_Invaders)])))
		zlim_C=c(0,round(max(outquant_Contractors[,2:ncol(outquant_Contractors)])))
		zlim_R=c(0,round(max(outquant_Retainers[,2:ncol(outquant_Retainers)])))
		
		png(paste(es,'_Invaders and Contractors_',yr,'.png',sep=''),width=dim(base.asc)[1]*2+200,height=dim(base.asc)[2]*2+50,units='px', pointsize=30, bg='lightgrey')
		par(mfrow=c(1,3),mar=c(2,2,2,2))
		
		pos=tpos
		pos=merge(pos,outquant_Invaders, by='SegmentNo',all.x=TRUE)
		cols=colorRampPalette(c("tan","forestgreen","darkblue"))(100)

		tasc=make.asc(pos[,paste(yr,'_50',sep='')])
		image(tasc,ann=F,axes=F,col=cols,zlim=zlim_I)
		mtext('Immigrants',side=1,cex=4)

}
color.legend(118,-42,140,-41,zlim,cols,cex=4)
dev.off()

