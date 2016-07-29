###### Script to clip fish for north - current
###### C. James (based on scripts by J VanDerWal)
################################################################################

sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
taxa=c('Fish','Crayfish','Turtles','Frogs'); tax=taxa[2] #change as appropriate
wd=paste(sdm.dir,'models_50_',tax,"/",sep="") ; setwd(wd)
species = list.files() #get a list of all the species
out.dir=paste('/home/jc246980/SDM/Realized/',tax,'/Clip4North/',sep='')
sh.dir='/home/jc246980/SDM/Realized/temp/'; setwd(sh.dir)
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/05.script2run_crays.r'

#write.csv(species,paste(wd,'cray_species_list.csv', sep='')) #write out species list


for (spp in species) {
spp.folder = paste(wd,spp,"/",sep=""); setwd(spp.folder) #define and set the species folder
spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',wd,'" ',sep='') # working directory argument
out.arg=paste('out.dir="',out.dir,'" ',sep='') # out directory argument


zz = file('05.clip_north_crays.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,out.arg,"' ",script.file,' 05.clip_north_crays.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' 05.clip_north_crays.sh -l pmem=1000mb -l walltime=00:01:00 -l nodes=1:ppn=3 -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))
Sys.sleep(2)

}

