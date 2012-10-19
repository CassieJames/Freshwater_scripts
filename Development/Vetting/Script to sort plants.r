#Script to sort out aquatic plants

work.dir="C:/Users/jc246980/Documents/Freshwater refugia project/Species data/ALA data/"   
ala.dir="C:/Users/jc246980/Documents/Freshwater refugia project/Species data/ALA data/Plants/Plant_families/"
families = list.files(ala.dir,pattern=".csv")
Plantlist=read.csv(paste(work.dir,"/","Plant_species2.csv",sep=""))   # load species list
species=(Plantlist$Species)



for (fam in 1:length(families)) { 

              fam.data = as.data.frame(read.csv(paste(ala.dir, '/',families[fam],sep='')))
              
        for (sp in 1:length(species)) { 

                   if(factor(species[sp]) %in% (fam.data$Matched.Scientific.Name)) {
                   plantsp <- (fam.data[which(as.character(fam.data$Matched.Scientific.Name) == as.character(species[sp])),])
                   write.csv(plantsp,paste("C:/Users/jc246980/Documents/Freshwater refugia project/Species data/ALA data/Plants/Plant_species/",gsub(' ','_',species[sp]),".csv", sep = ''), row.names = F )
                   } else {plantsp=NULL}
                   
        }
}




mappedspecies = list.files("C:/Users/jc246980/Documents/Freshwater refugia project/Species data/ALA data/Plants/Plant_species/",pattern=".csv")
mappedspecies=gsub('_',' ',mappedspecies);mappedspecies=gsub('.csv','',mappedspecies) 
missingspecies <- as.matrix(Plantlist$Species[!(Plantlist$Species %in% mappedspecies)])
write.csv(missingspecies,paste("C:/Users/jc246980/Documents/Freshwater refugia project/Species data/ALA data/Plants/","missing_plant_species.csv", sep = ''), row.names = F )


library(SDMTools); library(maptools)
source('/home/jc148322/scripts/libraries/cool_functions.r')
ala.dir="/home/jc246980/Species_data/ALA_downloads/Data/Aquatic_plants/Plant_species/"
image.dir="/home/jc246980/Species_data/ALA_downloads/Distribution_Images/Plant_distributions/"

load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

species=list.files(ala.dir)
for (sp in 1:length(species)) { cat(species[sp],'\n')


              species.data.ala = read.csv(paste(ala.dir, '/',species[sp],sep=''))
              species.data.ala=species.data.ala[,c("Matched.Scientific.Name","Latitude...processed","Longitude...processed")]
              species.data.ala=na.omit(species.data.ala)
              
              assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(species.data.ala$Longitude...processed,species.data.ala$Latitude...processed, padding.percent=10)

              setwd(image.dir)



        ## this creates the mini-australia, called ‘clip’
        if (max.lat>=-18 & min.lat<=-34 |
                        max.lon>=148 & min.lon<=120 ) {
                        xlim=c(min(pos$lon),max(pos$lon));
                        ylim=c(min(pos$lat),max(pos$lat))

        }else{
                        xlim=c(min.lon,max.lon);
                        ylim=c(min.lat,max.lat)
        }



              png(paste(species[sp],'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
                              par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))

                              mat = matrix(c( 2,3,3,3,
                                                                                              1,1,1,1,
                                                                                              1,1,1,1,
                                                                                              1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
                              layout(mat) #call layout as defined above

                              image(base.asc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)

                              image(base.asc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)

                              plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='grey93', lwd=1.5)
                              plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')

                              points(species.data.ala[,'Longitude...processed'],species.data.ala[,'Latitude...processed'],pch=16,cex=2, col='red', na.action = na.omit)

                              assign.list(l,r,b,t) %=% par("usr")

                              image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
                              image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)

                              plot(1:20,axes=FALSE,ann=FALSE,type='n')
                              speciesname=gsub('_',' ',species[sp]);speciesname=gsub('.csv',' ',speciesname)
                              text(7,15,speciesname,cex=4)

                              legend(5,10, inset = c(0.4, 0.2),
                              legend = c("ALA records"),
                              bty = "n", pch = c(19),           # bty = "n": no box
                              col = c(2), pt.cex = c(4),
                              lty = c(-1), cex=3)




              dev.off()
 }
}