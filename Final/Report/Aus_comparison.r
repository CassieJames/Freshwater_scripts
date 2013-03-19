library(SDMTools)
source('/home/jc148322/scripts/libraries/cool_functions.r')

raster=read.asc.gz('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz')
load('/home/jc148322/NARPfreshwater/SDM/obsolete/Env_layers/current.Rdata')
current=current[,c(1,ncol(current))]; colnames(current)=c('SegmentNo','flow')

pos=make.pos(raster)
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),raster)
pos=merge(pos,current,by='SegmentNo',all.x=TRUE)

base.asc=raster


##data for Janet's
library(maptools)
jflow=read.dbf('/home/jc246980/Janet_Stein_data/streamattributes.dbf')
jflow=jflow[,2:3]; colnames(jflow)=c('SegmentNo','jflow')

pos=merge(pos,jflow,by='SegmentNo',all.x=TRUE)
pos$jflow[which(is.na(pos$jflow))]=0
#make asciis
tasc=make.asc(pos$flow)
jasc=make.asc(pos$jflow)
# maxF=250000
# tasc[which(tasc>maxF)]=maxF
# jasc[which(jasc>maxF)]=maxF

zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(c(pos$flow,pos$jflow),na.rm=T))
# zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(tasc,na.rm=T))
cols = colorRampPalette(c('tan',"#E0F3F8","#ABD9E9","#90c4de","#74ADD1","#5d91c3","#4575B4","#4067ad","#3c58a6","#37489d","#313695","#191c90",'blue4','#0d0d7d','midnightblue','#131354','#0d0d39','#06071e'))(100)

pnts=cbind(x=c(145.5,147,147,145.5),y=c(-12,-12,-16,-16))

setwd('/home/jc246980/Final report/Figures/')
png('Aus_flow.png',width=dim(tasc)[1]*2+50,height=dim(tasc)[2]+50, bg='white')
par(mfrow=c(1,2),mar=c(4,1,1,3))
image(tasc, ann=F,axes=F,zlim=zlim,col=cols)
mtext('This report', side=1, cex=15)
image(jasc, ann=F,axes=F,zlim=zlim,col=cols)

mtext('Stein et al.(in prep)', side=1,cex=15)
legend.gradient(pnts,col=cols, round(zlim/1000), title='Flow (GL/yr)',cex=12)
dev.off()