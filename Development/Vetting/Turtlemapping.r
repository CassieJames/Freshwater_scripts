
library(SDMTools); library(maptools); library(stringr)#load the necessary libraries
source('/home/jc148322/scripts/libraries/cool_functions.r')

image.dir = "/home/jc246980/Species_data/ALA_downloads/Distribution_Images"          #location of images
turtledata.dir = "/home/jc246980/Species_data/ALA_downloads/Data/Turtles"
work.dir="/home/jc246980/Species_data/ALA_downloads"             # location of full fish species list
data.dir="/home/jc246980/Janet_Stein_data"       # location of catchment and river files
ala.data = "/home/jc214262/Refugia/Vert_data/ALA_Vertebrate_data"      #location of ALA files
CCTBC.dir = "/home/jc246980/Species_data/CCTBC_data"

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

TurtleList = as.data.frame(read.csv(paste(work.dir,"/","Full_Turtle_List.csv",sep=""), header = T,sep = ",", fill = T))   # load full species list
species=TurtleList$alternative_scientific_name

###################################
#subset CCTB data into turtle species

#cctbc.data = read.csv(paste(CCTBC.dir, '/',"CTBCC_verts_raw_nobirds.csv",sep=''))
#Turtles_CCTBC <- as.matrix(TurtleList$alternative_scientific_name[TurtleList$alternative_scientific_name %in% cctbc.data$species])  

#for (sp in 1:length(species)) { cat(species[sp],'\n')
#                   speciesname=gsub('_',' ',species[sp])
#                   turtle <- (cctbc.data[which(as.character(cctbc.data$species) ==as.character(speciesname)),])  
#                   write.csv(turtle,paste("/home/jc246980/Species_data/CCTBC_data/Data/",species[sp],".csv", sep = ''), row.names = F )

#        }


###################################
#subset UC data into turtle species

#UCturtledata.dir = "/home/jc246980/Species_data/UC_Wildlife_Tissue_Collection/UC_downloads"
#UCTurtles=list.files(UCturtledata.dir)
UCturtledata= "/home/jc246980/Species_data/UC_Wildlife_Tissue_Collection"



#for (sp in 1:length(UCTurtles)) { 
          
#          tdata = as.data.frame(read.csv(paste(UCturtledata.dir, '/',UCTurtles[sp],sep=''), header=FALSE))
#          spfile=NULL
#          spfile=cbind(spfile, as.character(tdata$V3))
#          DECLONG <- as.numeric(as.character(substr((str_split_fixed(tdata$V6, fixed(","), 2)[, 2]),1,nchar(str_split_fixed(tdata$V6, fixed(","), 2)[, 1])-1)))
#          spfile=cbind(spfile,DECLAT)
#          spfile=cbind(spfile,DECLONG)
#          colnames(spfile)=c('Species', 'DECLAT', 'DECLONG')
#          spfilenew=as.data.frame(spfile[1:nrow(spfile)-1,])
          
#          write.csv(spfilenew,paste(UCturtledata,"/", UCTurtles[sp],sep = ''), row.names = F )
#}
          
###################################
#Bring in all the necessary information

#catchments = readShapePoly('/home/jc246980/Janet_Stein_data/Level2Catchments/NCBLevel2Drainage.shp') #read in your shapefile
#rivers = readShapeLines('/home/jc246980/Janet Stein data/Major_rivers/Major_rivers.shp')
#save(rivers,file=paste(data.dir,'rivers.Rdata',sep=''))
#save(catchments,file=paste(data.dir,'catchments.Rdata',sep=''))
#load("/home/jc246980/Janet_Stein_data/Reaches.Rdata")
# Not bothered adjusting the zoom for the CCTBC data as there are so few records)

image.dir = "/home/jc246980/Species_data/ALA_downloads/Distribution_Images/Turtle_distributions/"     

load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")
species=list.files(turtledata.dir)

for (sp in 1:length(species)) { cat(species[sp],'\n')
              speciesname=gsub('_',' ',species[sp])
              species.data.ala = read.csv(paste(turtledata.dir, '/',species[sp],sep=''))

                              
              if(file.exists(paste(UCturtledata,"/", species[sp],sep=''))){
                              species.data.UC= read.csv(paste(UCturtledata,"/",species[sp],sep=''))
                              assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(rbind(species.data.ala$LONGDEC, species.data.UC$DECLONG),rbind(species.data.ala$LATDEC, (species.data.UC$DECLAT*-1)), padding.percent=10)
                              }
              
              else (assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(species.data.ala$LONGDEC,species.data.ala$LATDEC, padding.percent=10))
              
              
              
              
              
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

        setwd(image.dir)
   
              png(paste(gsub(".csv","",speciesname),'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
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

                              points(species.data.ala[,'LONGDEC'],species.data.ala[,'LATDEC'],pch=16,cex=2, col='red')

                              if(file.exists(paste(CCTBC.dir, '/Data/Turtles/',species[sp],sep=''))){
                              species.data.cctbc = read.csv(paste(CCTBC.dir, '/Data/Turtles/',species[sp],sep=''))
                              points(species.data.cctbc[,'LONGDEC'],species.data.cctbc[,'LATDEC'],pch=16,cex=2, col='green')
                              }
                              
                              if(file.exists(paste(UCturtledata,"/", species[sp],sep=''))){
                              species.data.UC= read.csv(paste(UCturtledata,"/",species[sp],sep=''))
                              points(species.data.UC[,'DECLONG'],species.data.UC[,'DECLAT']*-1,pch=16,cex=2, col='cyan')
                              }
                              
                              
                              
                              assign.list(l,r,b,t) %=% par("usr")

                              image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
                              image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)


                              plot(1:20,axes=FALSE,ann=FALSE,type='n')
                              text(7,15,gsub(".csv","",speciesname),cex=4)

                              legend(5,10, inset = c(0.4, 0.2),
                              legend = c("ALA records", "CTBCC records", "UC records"),
                              bty = "n", pch = c(19, 19,19),           # bty = "n": no box
                              col = c(2, 3,5), pt.cex = c(4, 4),
                              lty = c(-1, -1,-1), cex=3)
              dev.off()
 
}
