rm(list=ls()) 
library(SDMTools) #load the library
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/';setwd(wd)
taxa=c('amphibians','birds','mammals','reptiles') ; tax=taxa[2]
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

for(tax in taxa[2:4]){ print(tax)
###Invaders!
	tasc = read.asc.gz(paste(tax,'_invaders_proportional.asc.gz',sep=''))
		pos$tasc = extract.data(cbind(pos$lon, pos$lat),tasc) ; range(pos$tasc,na.rm=T)
		#pos$tasc1 = abs(pos$tasc -1) ; range(pos$tasc1,na.rm=T)
		if (tax==taxa[1]) {pos$tasc[which(pos$tasc>3)] = 3}
		quant = quantile(pos$tasc,0.9,na.rm=TRUE,type = 8);print(quant)
		pos$tasc2 = pos$tasc
		pos$tasc2[which(pos$tasc2>quant)] = 1; pos$tasc2[which(pos$tasc2<quant)] = 0 ;pos$tasc2[which(pos$tasc2==quant)] = 1
		leastchange_i = base.asc ; leastchange_i[cbind(pos$row,pos$col)]=pos$tasc2
				 	
		write.asc.gz(leastchange_i,paste("10_percentile/",tax,"_90thpc_of_proport_invaders.asc",sep=""))
		
###Contractors! 
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
	tasc = read.asc.gz(paste(tax,'_contractors_proportional.asc.gz',sep=''))
		pos$tasc = extract.data(cbind(pos$lon, pos$lat),tasc) ; range(pos$tasc,na.rm=T)
		quant = quantile(pos$tasc,0.1,na.rm=TRUE,type = 8);print(quant)
		pos$tasc2 = pos$tasc
		pos$tasc2[which(pos$tasc2>quant)] = 0; pos$tasc2[which(pos$tasc2>0)] = 1
		leastchange_c = base.asc ; leastchange_c[cbind(pos$row,pos$col)]=pos$tasc2
		
		write.asc.gz(leastchange_c,paste("10_percentile/",tax,"_10thpc_of_proport_contractors.asc",sep=""))
}

		
###### all taxa invaders
outdir = "10_percentile/";setwd(outdir)
	outasc = NULL
	atasc = read.asc.gz(paste("amphibians_90thpc_of_proport_invaders.asc.gz",sep=""))
	btasc = read.asc.gz(paste("birds_90thpc_of_proport_invaders.asc.gz",sep=""))
	mtasc = read.asc.gz(paste("mammals_90thpc_of_proport_invaders.asc.gz",sep=""))
	rtasc = read.asc.gz(paste("reptiles_90thpc_of_proport_invaders.asc.gz",sep=""))
	range(atasc,na.rm=T);range(btasc,na.rm=T);range(mtasc,na.rm=T);range(rtasc,na.rm=T)
	outasc = atasc + btasc + mtasc + rtasc
	range(outasc,na.rm=T)
	write.asc.gz(outasc,paste("all4tax_90thpc_proport_invaders.asc",sep=''))
	
###### all taxa contractors
	outasc = NULL
	atasc = read.asc.gz(paste("amphibians_10thpc_of_proport_contractors.asc.gz",sep=""))
	btasc = read.asc.gz(paste("birds_10thpc_of_proport_contractors.asc.gz",sep=""))
	mtasc = read.asc.gz(paste("mammals_10thpc_of_proport_contractors.asc.gz",sep=""))
	rtasc = read.asc.gz(paste("reptiles_10thpc_of_proport_contractors.asc.gz",sep=""))
	range(atasc,na.rm=T);range(btasc,na.rm=T);range(mtasc,na.rm=T);range(rtasc,na.rm=T)
	outasc = atasc + btasc + mtasc + rtasc
	range(outasc,na.rm=T)
	write.asc.gz(outasc,paste("all4tax_10thpc_proport_contractors.asc",sep=''))
	
############# ok pic

for(tax in taxa[2:4]) {	print(tax)
pic.dir = 'pics/'	
	tasci = read.asc.gz(paste(tax,"_90thpc_of_proport_invaders.asc.gz",sep=""))
	tascc = read.asc.gz(paste(tax,"_10thpc_of_proport_contractors.asc.gz",sep=""))
	zl = c(0,1)
	cols = c("grey75","blue")
	pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,tax,"_lowest_10th_I_E.png",sep=''),height=dim(tasci)[1]+50,width=dim(tasci)[2]*2+50,units='px',pointsize=20,bg= "white")
 	par(mfrow = c(1,2),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(tasci, axes=FALSE,ann=FALSE,col=cols,zlim=zl)
   	image(tascc, axes=FALSE,ann=FALSE,col=cols,zlim=zl)
    legend.gradient(pnts,cols=c(cols),limits=zl, title='10th pctl',cex=5.5)
	mtext(tax, side=3,at=0.5,cex=8,line=-18,outer=TRUE)
    mtext(c("Immigrants","Emigrants"), side=1,at=c(0.27,0.73),cex=8,line=-24,outer=TRUE)      
        
    dev.off()
}
########## all 4 summed together
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/10_percentile/';setwd(wd)	
alltax10 = read.asc.gz('alltax_10thpc_of_10th_rich_2085.asc.gz')
alltax50 = read.asc.gz('alltax_10thpc_of_median_rich_2085.asc.gz')
alltax90 = read.asc.gz('alltax_10thpc_of_90th_rich_2085.asc.gz')

zl = c(0,4)
	cols = c(colorRampPalette(c("grey75","yellow","green","blue","black"))(5))

	pnts=cbind(x=c(147.8,149.3,149.3,147.8), y=c(-13.2,-13.2,-17.6,-17.6))
	png(paste(pic.dir,"All_tax_lowest_spp_rich_turnover_each_percentile.png",sep=''),height=dim(atasc)[1]+50,width=dim(atasc)[2]*3+50,units='px',pointsize=20,bg= "white")
 	par(mfrow = c(1,3),oma=c(0,0,0,0),mar=c(0,0,0,0),cex=1,cex.axis=1)
    image(alltax10, axes=FALSE,ann=FALSE,col=cols,zlim=zl,cex=3)
  	image(alltax50, axes=FALSE,ann=FALSE,col=cols,zlim=zl,)
    image(alltax90, axes=FALSE,ann=FALSE,col=cols,zlim=zl,)
    legend.gradient(pnts,cols=c(cols),limits=zl, title='# taxa',cex=1.5)
	mtext("10th percentile of change in species richness", side=1,at=0.5,cex=3,line=-5,outer=TRUE)
    mtext(c("10th","50th",'90th'), side=3,at=seq(1/6,0.99,1/3),cex=3,line=-5,outer=TRUE)
        
    dev.off()

 ########### ok now finding all areas of NO turnover...
 wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/';setwd(wd)	
 allinv = read.asc.gz('all_taxa_invaders.asc.gz')
 allcont = read.asc.gz('all_taxa_contractors.asc.gz')
 range(allinv,na.rm=T); range(allcont,na.rm=T)
 
 taxa=c('amphibians','reptiles','mammals','birds') ; tax=taxa[1]

 for (tax in taxa) {print(tax)
 inv = read.asc.gz(paste(tax,'_invaders.asc.gz',sep=''))
 cont = read.asc.gz(paste(tax,'_contractors.asc.gz',sep=''))
 invr = range(inv,na.rm=T);contr = range(cont,na.rm=T)
 print(invr);print(contr)
 
 }
 
 
 
 
 
 
 
 
 
 
 
