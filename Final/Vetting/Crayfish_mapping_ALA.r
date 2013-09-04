### Code to create species files - don't run again as have deleted shrimps, crabs and species not generally recognised and with only limited records
#Script to sort out crayfish data from ALA data

# ala.dir="/home/jc246980/ALA_downloads/Data/Inverts"
# families = list.files(ala.dir,pattern=".csv")

# for (fam in 1:length(families)) { cat(families[fam],'\n')

              # fam.data = read.csv(paste(ala.dir, '/',families[fam],sep=''))
              # species=unique(fam.data$Matched_Scientific_Name)
              
        # for (sp in 1:length(species)) { cat(species[sp],'\n')

                   # yabbie <- fam.data[which(fam.data$Matched_Scientific_Name ==species[sp]),]
                   
                   # write.csv(yabbie,paste("/home/jc246980/ALA_downloads/Data/Inverts/Invert_species/",gsub(' ','_',species[sp]),".csv", sep = ''), row.names = F )

        # }
# }

# #Script to sort out crayfish data from river model records

# database.dir="/home/jc246980/Species_data/Crayfish_database"
# tdata = read.csv(paste(database.dir, '/',"CrayfishRivermodellingrecords.csv",sep=''))
# species=unique(tdata$Species)

      # for (sp in 1:length(species)) { cat(species[sp],'\n')

                   # yabbie <- tdata[which(tdata$Species ==species[sp]),]
                   
                   # write.csv(yabbie,paste("/home/jc246980/Species_data/Crayfish_database/River_models/",gsub(' ','_',species[sp]),".csv", sep = ''), row.names = F )

        # }


# #Script to sort out additional crayfish data from crayfish database

# database.dir="/home/jc246980/Species_data/Crayfish_database"
# craydata = read.csv(paste(database.dir, '/',"Additional_data_crayfish.csv",sep=''))
# species=colnames(craydata[,6:65])
		
		# for (sp in species) { cat(species[sp],'\n')
					
					# craysp=craydata[which(craydata[,sp]==1),1:5]
				    # write.csv(craysp,paste("/home/jc246980/Species_data/Crayfish_database/Additional_records/",sp,".csv", sep = ''), row.names = F )

		# }

# #Script to sort out additional museum data

# database.dir="/home/jc246980/Species_data/Crayfish_database"
# craydata = read.csv(paste(database.dir, '/',"Crayfish_museumdata.csv",sep=''))		
# species=unique(craydata$Species)	

		# for (sp in species) { cat(sp,'\n')
					
					# craysp=craydata[which(craydata$Species==sp),]
				    # write.csv(craysp,paste("/home/jc246980/Species_data/Crayfish_database/Museum_data/",gsub(' ','_',sp),".csv", sep = ''), row.names = F )

		# }

		


#Script to sort out additional Tasmanian museum data

# database.dir="/home/jc246980/Species_data/Crayfish_database"
# craydata = read.csv(paste(database.dir, '/',"Tasmanian_crayfish.csv",sep=''))		
# species=unique(craydata$Species)	

		# for (sp in species) { cat(sp,'\n')
					
					# craysp=craydata[which(craydata$Species==sp),]
				    # write.csv(craysp,paste("/home/jc246980/Species_data/Crayfish_database/Tasmanian_museum/",gsub(' ','_',sp),".csv", sep = ''), row.names = F )

		# }

		



library(SDMTools); library(maptools)
source('/home/jc148322/scripts/libraries/cool_functions.r')
image.dir="/home/jc246980/Species_data/Distribution_images/Crayfish/"
data.dir="/home/jc246980/Species_data/Crayfish_database/Merged_crayfish_records/"

ala.dir="/home/jc246980/Species_data/ALA_downloads/Data/Inverts/Invert_species/"
crayrivermodel="/home/jc246980/Species_data/Crayfish_database/River_models/"
craydatabase="/home/jc246980/Species_data/Crayfish_database/Additional_records/"
craydatamuseum="/home/jc246980/Species_data/Crayfish_database/Museum_data/"
craydataTasmuseum="/home/jc246980/Species_data/Crayfish_database/Tasmanian_museum/"

load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files



alaspecies=list.files(ala.dir)
rivermodelspecies=list.files(crayrivermodel)
databasespecies=list.files(craydatabase)
museumspecies=list.files(craydatamuseum)
tasmuseum=list.files(craydataTasmuseum)
species=unique(c(alaspecies, rivermodelspecies,databasespecies, museumspecies, tasmuseum)) 


write.csv(species,paste("/home/jc246980/Species_data/Crayfish_database/Full_crayfish_list.csv", sep = ''), row.names = F )


for (sp in 1:length(species)) { cat(species[sp],'\n')

		     if(file.exists(paste(ala.dir,'/',species[sp],sep=''))){
			 species.data.ala = read.csv(paste(ala.dir, '/',species[sp],sep=''))
             species.data.ala=species.data.ala[,c("Matched_Scientific_Name","Latitude_processed","Longitude_processed")]
			 colnames(species.data.ala)=c("spname","LAT", "LONG") 
             species.data.ala=na.omit(species.data.ala)
			 }else{species.data.ala=matrix(NA,nrow=1,ncol=3); colnames(species.data.ala)= c("spname","LAT", "LONG")}

		     if(file.exists(paste(crayrivermodel, '/',species[sp],sep=''))){
			 species.data.river = read.csv(paste(crayrivermodel, '/',species[sp],sep=''))
             species.data.river=species.data.river[,c("Species","LATDEC","LONGDEC")]
             colnames(species.data.river)=c("spname","LAT", "LONG") 
			 species.data.river[,"LAT"]=species.data.river[,"LAT"]*-1
			 species.data.river=na.omit(species.data.river)
			 }else{species.data.river=matrix(NA,nrow=1,ncol=3); colnames(species.data.river)= c("spname","LAT", "LONG")}
			  
			 if(file.exists(paste(craydatabase, '/',species[sp],sep=''))){ 
			  species.data.dbase = read.csv(paste(craydatabase, '/',species[sp],sep=''))
              species.data.dbase=species.data.dbase[,c("Citation","Lat","Long")]		  
			  colnames(species.data.dbase)=c("Citation","LAT", "LONG") 
              species.data.dbase=na.omit(species.data.dbase)
			 }else{species.data.dbase=matrix(NA,nrow=1,ncol=3); colnames(species.data.dbase)= c("spname","LAT", "LONG")}
			 
			  if(file.exists(paste(craydatamuseum, '/',species[sp],sep=''))){ 
			  species.data.museum = read.csv(paste(craydatamuseum, '/',species[sp],sep=''))
              species.data.museum=species.data.museum[,c("Species","LAT","LON")]
              colnames(species.data.museum)=c("spname","LAT", "LONG") 
			  species.data.museum[,"LAT"]=species.data.museum[,"LAT"]*-1
			  species.data.museum=na.omit(species.data.museum)			  
			 }else{species.data.museum=matrix(NA,nrow=1,ncol=3); colnames(species.data.museum)= c("spname","LAT", "LONG")}	  
			 
			   if(file.exists(paste(craydataTasmuseum, '/',species[sp],sep=''))){ 
			  species.data.Tasmuseum = read.csv(paste(craydataTasmuseum, '/',species[sp],sep=''))
              species.data.Tasmuseum=species.data.Tasmuseum[,c("Species","PublishLatitude","PublishLongitude")]
              colnames(species.data.Tasmuseum)=c("spname","LAT", "LONG") 
			  species.data.Tasmuseum[,"LAT"]=species.data.Tasmuseum[,"LAT"]
			  species.data.Tasmuseum=na.omit(species.data.Tasmuseum)			  
			 }else{species.data.Tasmuseum=matrix(NA,nrow=1,ncol=3); colnames(species.data.Tasmuseum)= c("spname","LAT", "LONG")}	  
			  
			
			species_data=matrix(NA,nrow=1,ncol=2); colnames(species_data)= c("LAT", "LONG")
			species_data=rbind(species_data, species.data.ala[,2:3])
			species_data=rbind(species_data, species.data.river[,2:3])
			species_data=rbind(species_data, species.data.dbase[,2:3])
			species_data=rbind(species_data, species.data.museum[,2:3])
			species_data=rbind(species_data, species.data.Tasmuseum[,2:3])
			
			species_data=na.omit(species_data)		

			
			write.csv(species_data,paste(data.dir,species[sp], sep = ''), row.names = F )
			 
			assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(species_data$LONG,species_data$LAT, padding.percent=10)

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




              png(paste(gsub('.csv','',species[sp]),'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
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
							  points(species_data[,'LONG'],species_data[,'LAT'],pch=16,cex=2, col='black')
							  
							  assign.list(l,r,b,t) %=% par("usr")

                              image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
                              image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)
							  
							  speciesname=gsub('_',' ',species[sp])
							  speciesname=gsub('.csv',' ',speciesname)
							  plot(1:20,axes=FALSE,ann=FALSE,type='n')
                              text(7,15,speciesname,cex=4)

              dev.off()
 }
 
