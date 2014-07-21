######################################################################
### Script to batch out distribution images for vetting of current clipped to basin or bioregion
### C. James 												27th February 2014


sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
taxa=c('fish','crayfish','turtles','All_frog'); tax=taxa[1] #change as appropriate
work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)

species = list.files("/home/jc246980/SDM/Realized_current/fish/Clipped2Basin") #get a list of all the species

image.dir=paste('/home/jc246980/SDM/Realized_current/Images/',tax,'/',sep='')
sh.dir='/home/jc246980/SDM/Realized_current/Images/temp/'; setwd(sh.dir)
#work.dir=(paste('/home/jc246980/SDM/Realized_current/',tax,'/',sep=''))	

image=paste('/home/jc246980/SDM/Realized_current/Images/',tax,'/',sep='')

wd=(paste('/home/jc246980/SDM/Realized_current/',tax,'/Clipped2Basin/',sep=''))	

script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/99.images_current_distribution.r'


for (spp in species) {


spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',wd,'" ',sep='') # working directory argument
image.arg=paste('image="',image.dir,'" ',sep='') # image directory argument

zz = file('99.images_current_distribution.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,image.arg,"' ",script.file,' 99.images_current_distribution.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' 99.images_current_distribution.sh -l pmem=2500mb -l nodes=1:ppn=3  -l walltime=00:12:00',sep=''))
Sys.sleep(5)
}
