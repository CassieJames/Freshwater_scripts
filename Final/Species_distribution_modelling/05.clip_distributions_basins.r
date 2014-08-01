###### Script to clip to river basins for all fish for continental scale analysis
###### C. James (based on scripts by J VanDerWal)
################################################################################

sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] #change as appropriate
wd=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(wd)
species = list.files() #get a list of all the species
out=paste('/home/jc246980/SDM/Realized/',tax,'/Clipped2Basin/',sep='')
sh.dir='/home/jc246980/SDM/Realized/temp/'; setwd(sh.dir)
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/05.script2run_basins.r'
ESs=c('RCP3PD','RCP45','RCP6','RCP85')
es=ESs[4]
for (es in ESs) {

for (spp in species) {
spp.folder = paste(wd,spp,"/",sep=""); setwd(spp.folder) #define and set the species folder
spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',wd,'" ',sep='') # working directory argument
out.arg = paste('out="',out,'" ',sep='') # out directory argument
es.arg = paste('es="',es,'" ',sep='')

zz = file('04.clip_basins.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,out.arg,es.arg,"' ",script.file,' 04.clip_basins.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' 05.clip_basins.sh -l pmem=1000mb -l walltime=00:5:00 -l nodes=1:ppn=3 -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))
Sys.sleep(2)

}
}
