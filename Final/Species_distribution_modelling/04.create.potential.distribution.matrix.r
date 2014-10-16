#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################

taxa = c("fish", "crayfish","frog","turtles")
tax = taxa[2]	
wd = paste('/home/jc246980/SDM/models_',tax,"/",sep=""); setwd(wd)

script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/04.script2run.r'

species = list.files() #get a list of all the species


for (spp in species) {
spp.folder = paste(wd,spp,"/",sep=""); setwd(spp.folder) #define and set the species folder
spp.arg = paste('spp="',spp,'" ',sep='')
wd.arg = paste('wd="',wd,'" ',sep='')

zz = file('04.create.pot.mat.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R/2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",spp.arg,wd.arg,"' ",script.file,' 04.create.pot.mat.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,' -l pmem=1000mb -l walltime=24:00:00 -l nodes=1:ppn=3 -l epilogue=/home/jc246980/epilogue/epilogue.sh 04.create.pot.mat.sh',sep=''))
}

