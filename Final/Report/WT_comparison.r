library(SDMTools)
source('/home/jc148322/scripts/libraries/cool_functions.r')

raster=read.asc('/home/jc148322/NARPfreshwater/SegmentNo_AWT.asc')
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

#make asciis
tasc=make.asc(pos$flow)
jasc=make.asc(pos$jflow)
# maxF=250000
# tasc[which(tasc>maxF)]=maxF
# jasc[which(jasc>maxF)]=maxF

zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(c(pos$flow,pos$jflow),na.rm=T))
# zlim=c(min(c(pos$flow,pos$jflow),na.rm=T),max(tasc,na.rm=T))
# cols = colorRampPalette(c('tan',"#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695",'blue4','midnightblue'))(100)
cols = colorRampPalette(c('tan',"#E0F3F8","#ABD9E9","#90c4de","#74ADD1","#5d91c3","#4575B4","#4067ad","#3c58a6","#37489d","#313695","#191c90",'blue4','#0d0d7d','midnightblue','#131354','#0d0d39','#06071e'))(100)
pnts=cbind(x=c(146,146.25,146.25,146),y=c(-16.5,-16.5,-16,-16))

setwd('/home/jc246980/Final report/Figures/')
png('WT_flow.png',width=dim(tasc)[1]*2+50,height=dim(tasc)[2]+70)
par(mfrow=c(1,2),mar=c(4,1,1,1))
image(tasc, ann=F,axes=F,zlim=zlim,col=cols)
mtext('This report', side=1, line=2,cex=3)
image(jasc, ann=F,axes=F,zlim=zlim,col=cols)
mtext('Stein et al. (in prep)', side=1,line=2,cex=3)
legend.gradient(pnts,col=cols, round(zlim/1000), title='Flow (GL/yr)',cex=3)
dev.off()

#colorRampPalette(c('tan', 'lightblue', 'slateblue', 'blue', 'blue4', 'black'))(101)