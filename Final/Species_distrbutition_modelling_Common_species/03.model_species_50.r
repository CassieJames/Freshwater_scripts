###written by Lauren Hodgson, modified from Jeremy VanDerWal's SDM scripts. Jan 2013
########################################################################################################
### Define inputs, working directories and necessary libraries
taxa=c('Fish','Crayfish','Turtles','Frogs')
taxon=taxa[4] #change as appropriate
taxname=sub(substr(taxon,1,1),toupper(substr(taxon,1,1)),taxon)


occur.file=paste('/home/jc246980/Species_data/Reach_data/',taxname,"_reach_master_50.Rdata",sep='') #give the full file path of your species data

basedir='/home/jc246980/SDM/'
env.file=paste(basedir,"current_1990.Rdata",sep='')
maxent.jar = "/home/jc246980/SDM/maxent.jar" #define the location of the maxent.jar file
wd=paste("/home/jc246980/SDM/models_50_",taxon,"/",sep='');dir.create(wd); setwd(wd) #define and set the working directory
projdir = paste(basedir,'Environmental_future/',sep='') #define the projection directory
projs = list.files(projdir) #get a list of the projections
poi=projs[intersect(grep(2085,projs),grep('RCP85',projs))] # this command limits modelling to a limited number of projections
poi=c("current_1990.csv", poi)


########################################################################################################

load(env.file) #read in enviromental data
##########run only the first time
for (tt in paste('bioclim_',sprintf('%02i',c(1,4:11)),sep="")) { cat(tt,'\n') #round temperature where appropriate
cois = grep(tt,colnames(current)); current[,cois] = round(current[,cois],1)
}
for (tt in paste('bioclim_',sprintf('%02i',c(12:14,16:19)),sep="")) { cat(tt,'\n')#round precip where appropriate
 cois = grep(tt,colnames(current)); current[,cois] = round(current[,cois])
}
save(current,file=paste(basedir,'current_1990.Rdata',sep='')) #write out the full current dataset
write.csv(current,paste(projdir,'current_1990.csv',sep=''), row.names=FALSE) #write out the full current dataset

current = current[,c('SegmentNo','lat','lon',paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep=''),'max.clust.length','clust.severity','Flow_accum_annual', 'Segslope', 'Catslope', 'd2outlet')] #keep only variables of interest
load(occur.file); #load in the occur
species=colnames(occur)[-1] #get a list of the species

tt = unique(round(c(which(rowSums(occur[,-1])>=1),runif(20000,1,nrow(current))))) #get segment numbers where a species has been observed and append some 15k random
bkgd = current[tt,]; write.csv(bkgd,paste('../bkgd_',taxon,'.csv',sep=''),row.names=FALSE) #write out the background points

for (spp in species) {cat(spp,'\n')
if (length(which(occur[,spp]>0)) >= 50) { #model species where count is greater or equal to 50
toccur = current[which(current$SegmentNo %in% occur[which(occur[,spp]>0),'SegmentNo']),] #get segment number for the species
toccur$SegmentNo=spp #reset the values the species column of the occur file
spp.dir = paste(wd,spp,'/',sep='') #define the species directory
dir.create(paste(spp.dir,'output/potential/',sep=''),recursive=TRUE) #create the species directory and dir for all outputs
write.csv(toccur,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE) #write out the file

# create the shell script which will run the modelling jobs
zz = file(paste(spp.dir,'01.',spp,'.model.sh',sep=''),'w') #create the shell script to run the maxent model
cat('#!/bin/bash\n',file=zz)
cat('cd ',spp.dir,'\n',sep='',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz) #this line is necessary for 'module load' to work in tsch
cat('module load java\n',file=zz)
cat('java -mx2048m -jar ',maxent.jar,' -e ',basedir,'bkgd_',taxon,'.csv -s occur.csv -o output nothreshold nowarnings novisible replicates=10 -r -a \n',sep="",file=zz) #run maxent bootstrapped to get robust model statistics
cat('cp -af output/maxentResults.csv output/maxentResults.crossvalide.csv\n',file=zz) #copy the maxent results file so that it is not overwritten
cat('java -mx2048m -jar ',maxent.jar,' -e ',basedir,'bkgd_',taxon,'.csv -s occur.csv -o output nothreshold nowarnings novisible nowriteclampgrid nowritemess writeplotdata -P -J -r -a \n',sep="",file=zz) #run a full model to get the best parameterized model for projecting
for (tproj in projs) cat('java -mx2048m -cp ',maxent.jar,' density.Project ',spp.dir,'output/',spp,'.lambdas ',projdir,tproj,' ',spp.dir,'output/potential/',tproj,' fadebyclamping nowriteclampgrid\n',sep="",file=zz) #do the projections
close(zz)
setwd(spp.dir); system(paste('qsub -m n 01.',spp,'.model.sh -l pmem=8500mb -l walltime=24:00:00 -l nodes=1:ppn=1', sep='')); setwd(wd) #submit the script
}
}

