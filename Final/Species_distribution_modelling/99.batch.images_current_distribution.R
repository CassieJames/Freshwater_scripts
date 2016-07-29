######################################################################
### Script to batch out distribution images for vetting of current clipped to basin or bioregion
### C. James 												27th February 2014


taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] #change as appropriate
image.dir=paste('/home/jc246980/SDM/Realized/Images/',tax,'/Clip4North/',sep='')
wd=paste('/home/jc246980/SDM/Realized/',tax,'/Clip4North/',sep=''); setwd(wd)
files=list.files(wd)
species=files[grep('cur.real.mat',files)]
sh.dir='/home/jc246980/SDM/Realized/Images/temp/'; setwd(sh.dir)
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/99.images_current_distribution.r'

ESs=c('RCP3PD','RCP45','RCP6','RCP85')
es="RCP85"

for (spp in species) {


spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',wd,'" ',sep='') # working directory argument
image.arg=paste('image.dir="',image.dir,'" ',sep='') # image directory argument
es.arg = paste('es="',es,'" ',sep='')

zz = file('99.images_current_distribution.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,image.arg,es.arg,"' ",script.file,' 99.images_current_distribution.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' 99.images_current_distribution.sh -l pmem=2500mb -l nodes=1:ppn=3  -l walltime=12:00:00 -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))
Sys.sleep(2)
}

