###### Script to clip to river basins of fish bioregions
###### C. James (based on scripts by J VanDerWal)
################################################################################

sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
taxa=c('fish','crayfish','turtles','frog'); tax=taxa[1] #change as appropriate
work.dir=paste(sdm.dir,'models_',tax,"/",sep="") ; setwd(work.dir)
species = list.files() #get a list of all the species

out.dir=paste('/home/jc246980/SDM/Realized_current/',tax,'/Clipped2Basin/',sep='')


sh.dir='/home/jc246980/SDM/Realized_current/temp/'; setwd(sh.dir)

script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/05.script2run_fish.r'




for (spp in species) {
spp.folder = paste(work.dir,spp,"/",sep=""); setwd(spp.folder) #define and set the species folder
spp.arg = paste('spp="',spp,'" ',sep='') # species argument
wd.arg = paste('wd="',work.dir,'" ',sep='') # working directory argument
out.arg=paste('out="',out.dir,'" ',sep='') # out directory argument


zz = file('04.clip_distributions.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,out.arg,clip.arg,"' ",script.file,' 04.clip_distributions.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' -l nodes=1:ppn=3 04.clip_distributions.sh',sep=''))
Sys.sleep(5)

}

