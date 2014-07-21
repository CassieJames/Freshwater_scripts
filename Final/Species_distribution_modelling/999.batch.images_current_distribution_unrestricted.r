######################################################################
### Script to batch out distribution images for vetting of current clipped to basin or bioregion
### C. James 												27th February 2014


sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
taxa=c('fish','crayfish','turtles','All_frog'); tax=taxa[3] #change as appropriate
work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
image.dir=paste('/home/jc246980/SDM/Realized_current/Images/',tax,'/Unrestricted/',sep='')
sh.dir='/home/jc246980/SDM/Realized_current/Images/temp/'; 

# Code for frogs
#unrestrict=read.csv("/home/jc246980/SDM/Frogs_underpredicted.csv")
#species=unrestrict[,1]
species = list.files() #get a list of all the species
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/999.images_current_distribution_unrestricted.r'

setwd(sh.dir)

for (spp in species) {

spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',work.dir,'" ',sep='') # working directory argument
image.arg=paste('image="',image.dir,'" ',sep='') # image directory argument

zz = file('999.images_current_distribution.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,image.arg,"' ",script.file,' 999.images_current_distribution.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' -l nodes=1:ppn=3 999.images_current_distribution.sh',sep=''))
Sys.sleep(5)
}