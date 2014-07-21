########## invaders and contractors proportional

library(SDMTools)#load the necessary libraries
taxa = c("amphibians", "birds","mammals","reptiles") ;	tax = taxa[1]
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/';setwd(wd)
pic.dir = 'pics/'
for (tax in taxa[3:4]) {
	inv = read.asc.gz(paste(tax,"_invaders.asc.gz",sep=''))
	cont= read.asc.gz(paste(tax,"_contractors.asc.gz",sep=''))
	range(inv,na.rm=T); 
	range(cont,na.rm=T)
	cur_amph = read.asc.gz(paste('/home/jc214262/Refugia/species_richness/1km/spp_rich/',tax,'/',tax,'_richness_1990.asc.gz',sep=''))
	range(cur_amph,na.rm=T)
	inv_proport = inv/cur_amph
	cont_proport = cont/cur_amph
	range(inv_proport,na.rm=T)
	range(cont_proport,na.rm=T)
	write.asc.gz(inv_proport,paste(tax,'_invaders_proportional.asc',sep='')) 
	write.asc.gz(cont_proport,paste(tax,'_contractors_proportional.asc',sep=''))
	}
	
	
#for (tax in taxa) { #
tax = taxa[1]
	invp = read.asc.gz(paste(tax,"_invaders_proportional.asc.gz",sep=''))
	contp= read.asc.gz(paste(tax,"_contractors_proportional.asc.gz",sep=''))
	
		range(invp,na.rm=T) ; range(contp,na.rm=T)
		contp[which(contp>1)]=1
		invp[which(invp>3)]=3
		
tt = invp[which(is.finite(invp))];tt=as.numeric(as.character(cut(tt,breaks=seq(0,3,0.1),labels = seq(0,2.9,0.1)))) ;invp[which(is.finite(invp))] = tt
range(invp,na.rm=T)  

	zlimitsi = c(0,3); zlimitsc=c(0,1)
	zl = c(zlimitsi[1],paste('>',zlimitsi[2],sep=''))
	
 cols = c(colorRampPalette(c('red','yellow'))(10),'grey',colorRampPalette(c('lightblue','darkblue','forestgreen'))(26))
  cols2= colorRampPalette(c("#006400","#008B00","#458B00","#9ACD32","#CDCD00","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#8B4726"))(10) #green - brown

	pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_invaders&contractors_proportional2.png",sep=''),height=dim(invp)[1]+50,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
  	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
	legend.gradient(pnts,cols=c(cols),limits=zl, title='Change',cex=5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='Change',cex=5)
	mtext(tax,side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
          
    dev.off()

############################################# birds
tax = taxa[2]
	invp = read.asc.gz(paste(tax,"_invaders_proportional.asc.gz",sep=''))
	contp= read.asc.gz(paste(tax,"_contractors_proportional.asc.gz",sep=''))

		range(invp,na.rm=T) ; range(contp,na.rm=T)
		
tt = invp[which(is.finite(invp))];tt=as.numeric(as.character(cut(tt,breaks=seq(0,1.1,0.1),labels = seq(0,1,0.1)))) ;invp[which(is.finite(invp))] = tt
	range(invp,na.rm=T) ; range(contp,na.rm=T)
	zlimitsi = c(0,1.1); zlimitsc = c(0,0.55) 
	zl = c(zlimitsi[1],paste('>',zlimitsi[2],sep=''))
 cols = c(colorRampPalette(c('red','orange','yellow'))(10),'grey',colorRampPalette(c('lightblue','darkblue','forestgreen'))(11))
 #cols2= colorRampPalette(c("#8B4726","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#CDCD00","#9ACD32","#458B00","#008B00","#006400"))(10) #brown - green
 cols2= colorRampPalette(c("#006400","#008B00","#458B00","#9ACD32","#CDCD00","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#8B4726"))(10) #green - brown

pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_invaders&contractors_proportional2.png",sep=''),height=dim(invp)[1]+50,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
  	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
	legend.gradient(pnts,cols=c(cols),limits=zl, title='Change',cex=5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='Change',cex=5)
	mtext(tax,side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
dev.off()
############################################# mammals
tax = taxa[3] ; print(tax)
	invp = read.asc.gz(paste(tax,"_invaders_proportional.asc.gz",sep=''))
	contp= read.asc.gz(paste(tax,"_contractors_proportional.asc.gz",sep=''))

	range(invp,na.rm=T) ; range(contp,na.rm=T)
		
tt = invp[which(is.finite(invp))];tt=as.numeric(as.character(cut(tt,breaks=seq(0,1.7,0.1),labels = seq(0,1.6,0.1)))) ;invp[which(is.finite(invp))] = tt
	zlimitsi = c(0,1.7); zlimitsc = c(0,0.8) 
	zl = c(zlimitsi[1],paste('>',zlimitsi[2],sep=''))
 cols = c(colorRampPalette(c('red','orange','yellow'))(10),'grey',colorRampPalette(c('lightblue','darkblue','forestgreen'))(11))
  cols2= colorRampPalette(c("#006400","#008B00","#458B00","#9ACD32","#CDCD00","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#8B4726"))(10) #green - brown

pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_invaders&contractors_proportional2.png",sep=''),height=dim(invp)[1]+50,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
  	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
	legend.gradient(pnts,cols=c(cols),limits=zl, title='Change',cex=5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='Change',cex=5)
	mtext(tax,side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
dev.off()
############################################# reptiles
tax = taxa[4] ; print(tax)
	invp = read.asc.gz(paste(tax,"_invaders_proportional.asc.gz",sep=''))
	contp= read.asc.gz(paste(tax,"_contractors_proportional.asc.gz",sep=''))

	range(invp,na.rm=T) ; range(contp,na.rm=T)
		
tt = invp[which(is.finite(invp))];tt=as.numeric(as.character(cut(tt,breaks=seq(0,1.1,0.1),labels = seq(0,1.0,0.1)))) ;invp[which(is.finite(invp))] = tt
	range(invp,na.rm=T)
	zlimitsi = c(0,1.1); zlimitsc = c(0,0.8)
zl = c(zlimitsi[1],paste('>',zlimitsi[2],sep=''))	
 cols = c(colorRampPalette(c('red','orange','yellow'))(10),'grey',colorRampPalette(c('lightblue','darkblue','forestgreen'))(11))
  cols2= colorRampPalette(c("#006400","#008B00","#458B00","#9ACD32","#CDCD00","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#8B4726"))(10) #green - brown

pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_invaders&contractors_proportional2.png",sep=''),height=dim(invp)[1]+50,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
  	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
	legend.gradient(pnts,cols=c(cols),limits=zl, title='Change',cex=5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='Change',cex=5)
	mtext(tax,side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
dev.off()
############################################# all taxa
outdir = '4_taxa_combined/';setwd(outdir)	
inv = read.asc.gz("all_taxa_invaders.asc.gz")
cont= read.asc.gz("all_taxa_contractors.asc.gz")
cur_rich = read.asc.gz("/home/jc214262/Refugia/species_richness/1km/spp_rich/all_taxa_pics/all_spp_richness_1990.asc.gz")

inv_proport = inv/cur_rich
cont_proport = cont/cur_rich

write.asc.gz(inv_proport,'All_taxa_invaders_proportional.asc')
write.asc.gz(cont_proport,'All_taxa_contractors_proportional.asc')

invp = read.asc.gz('All_taxa_invaders_proportional.asc.gz')
contp = read.asc.gz('All_taxa_contractors_proportional.asc.gz')

 #invp = inv_proport ; contp = cont_proport
range(invp,na.rm=T) ; range(contp,na.rm=T)
	tt = invp[which(is.finite(invp))];tt=as.numeric(as.character(cut(tt,breaks=seq(0,1,0.1),labels = seq(0,.9,0.1)))) ;invp[which(is.finite(invp))] = tt
	range(invp,na.rm=T)
	zlimitsi = c(0,1); zlimitsc = c(0,0.7) 
	zl = c(zlimitsi[1],paste('>',zlimitsi[2],sep=''))	
 cols = c(colorRampPalette(c('red','orange','yellow'))(10),'grey',colorRampPalette(c('lightblue','darkblue','forestgreen'))(11))
  cols2= colorRampPalette(c("#006400","#008B00","#458B00","#9ACD32","#CDCD00","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#8B4726"))(10) #green - brown
	
pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png("All_taxa_immigrants&emigrants_proportional2.png",height=dim(invp)[1]+30,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
 	par(mfrow = c(1,2),oma=c(3,3,4,1),mar=c(0,0,0,0))#,cex=.8)#,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
    legend.gradient(pnts,cols=c(cols),limits=zl, title='Change',cex=5.5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='Change',cex=5.5)
    mtext("All species",side=3,at=.5,cex=8,line=-8,outer=TRUE)
	mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-17,outer=TRUE)
          
    dev.off()

	