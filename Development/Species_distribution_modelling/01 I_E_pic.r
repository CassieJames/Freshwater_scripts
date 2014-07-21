rm(list=ls()) 
library(SDMTools)#load the necessary libraries
taxa = c("amphibians", "birds","mammals","reptiles") ;	tax = taxa[1]
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/';setwd(wd)
for (tax in taxa[3:4]){print(tax) 
inv = read.asc.gz(paste(tax,"_invaders.asc.gz",sep=''))
cont= read.asc.gz(paste(tax,"_contractors.asc.gz",sep=''))
	pic.dir = 'pics/'
	##make the pretty picture
	r90 =range(inv,na.rm=T) ;  r85 = range(cont,na.rm=T); zz = c(r90,r85);zs = sort(zz,decreasing = F); zl = c(zs[1],zs[4]); zmax = zs[4]; print(zl)
	zll = round(zl, digits=0)
zlimits =zl #c(0,87)
cols2 = c(colorRampPalette(c("gray","thistle2","rosybrown","orangered3","coral2","salmon","orange","darkgoldenrod1","yellow","olivedrab3","forestgreen","mediumseagreen","turquoise","turquoise3","deepskyblue1","dodgerblue","blue","blue3","darkblue","purple3","purple","black"))(zmax))

    pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_immigrants&emigrants.png",sep=''),height=dim(inv)[1]+50,width=dim(inv)[2]*2+50,units='px',pointsize=20,bg= "white")
 	 par(mfrow = c(1,2),oma=c(2.5,3,2,0.5),mar=c(0,0,0,0),cex=1,cex.axis=1)
     image(inv, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimits)
     image(cont, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimits)
    # # image(tasc2, axes=FALSE,ann=FALSE,col=cols,zlim=c(0,83))
     legend.gradient(pnts,cols=c(cols2),limits=zlimits, title='# species',cex=5)
     mtext(tax,side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
          
    dev.off()
	}
###example.........
	pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_invaders&contractors_proportional.png",sep=''),height=dim(invp)[1]+50,width=dim(invp)[2]*2+50,units='px',pointsize=20,bg= "white")
  	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(invp, axes=FALSE,ann=FALSE,col=cols,zlim=zlimitsi)
	legend.gradient(pnts,cols=c(cols),limits=zlimitsi, title='# species',cex=5)
    image(contp, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimitsc)
    legend.gradient(pnts,cols=c(cols2),limits=zlimitsc, title='# species',cex=5)
	mtext(tax,side=3,at=.5,cex=8,line=-8,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-17,outer=TRUE)
dev.off()

	
####################################################################################################################################################################
######### all taxa

library(SDMTools)#load the necessary libraries
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/4_taxa_combined/';setwd(wd)

inv = read.asc.gz("all_taxa_invaders.asc.gz")
cont= read.asc.gz("all_taxa_contractors.asc.gz")
	pic.dir = 'pics/';setwd(pic.dir)
	##make the pretty picture
	range(inv,na.rm=T) ; range(cont,na.rm=T)
#	zlimits = range(contractors,na.rm=T) ; smax = max(contractors,na.rm=T)
	zlimits =c(0,213)
cols2 = c(colorRampPalette(c("gray","thistle2","rosybrown","orangered3","coral2","salmon","orange","darkgoldenrod1","yellow","olivedrab3","forestgreen","mediumseagreen","turquoise","turquoise3","deepskyblue1","dodgerblue","blue","blue3","darkblue","purple3","purple","black"))(200))
    
	pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png("all_taxa_immigrants&emigrants.png",height=dim(inv)[1]+30,width=dim(inv)[2]*2+50,units='px',pointsize=20,bg= "white")
	 par(mfrow = c(1,2),oma=c(2.5,3,4,0.5),mar=c(0,0,0,0),cex=1,cex.axis=1)
     image(inv, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimits)
     image(cont, axes=FALSE,ann=FALSE,col=cols2,zlim=zlimits)
     legend.gradient(pnts,cols=c(cols2),limits=zlimits, title='# species',cex=5.5)
     mtext("All species",side=3,at=.5,cex=8,line=-18,outer=TRUE)
	 mtext(c("Immigrants","Emigrants"), side=1,at=c(0.2,0.73),cex=8,line=-24,outer=TRUE)
          
    dev.off()
