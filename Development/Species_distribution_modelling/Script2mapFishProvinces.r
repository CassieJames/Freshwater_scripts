	
### map of fish provinces to check 
image.dir=("/home/jc246980/SDM/")
png(paste(image.dir,"Fish_province_mapv2.png"),width=dim(base.asc)[1]*3+300, height=dim(base.asc)[2]*4+150, units='px', pointsize=20, bg='WHITE')	

mat = matrix(c( 3,2,2,2,
			  1,1,1,1,
			  1,1,1,1,
			  1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
layout(mat) #call layout as defined above

#pos=tpos
#pos=merge(pos,clip, by='SegmentNo',all.x=TRUE)
zlim=c(min(pos$Clip2Bio[which(pos$Clip2Bio>0)],na.rm=T),max(c(pos$Clip2Bio),na.rm=T))
tasc=make.asc(pos[,'Clip2Bio'])
image(tasc,ann=F,axes=F,col=cols,zlim=zlim)
dev.off()