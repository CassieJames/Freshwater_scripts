#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
tax=c('fish','crayfish','turtles','frog')
taxon=tax[1] #change as appropriate

sdm.dir='/home/jc246980/SDM/'
cur.dir=paste('/home/jc246980/SDM/Realized/',taxon,'/Clip4North/',sep='')
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/06.script2run_fish.r'
sh.dir='/home/jc246980/SDM/Realized/temp/'; setwd(sh.dir)
wd=paste(sdm.dir,'models_',taxon,"/",sep="")

exclude=read.csv('/home/jc246980/SDM/fish.to.exclude.csv',as.is=TRUE)
exclude=exclude[which(exclude[,2]=='exclude'),1]
species=list.files(wd)
species=setdiff(species,exclude)

ESs=c('RCP3PD','RCP45','RCP6','RCP85')

for (es in ESs) {

for (spp in species) {cat (spp,'\n')

#arguments
es.arg = paste('es="',es,'" ',sep='')
wd.arg = paste('wd="',wd,'" ',sep='')
tax.arg = paste('taxon="',taxon,'" ',sep='')
cur.arg=paste('cur.dir="',cur.dir,'" ',sep='') 
out.arg=paste('out="',out.dir,'" ',sep='') 

#create sh file
zz = file(paste('06.',es,'.future_realised.sh',sep=''),'w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R-2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",es.arg,tax.arg,wd.arg,cur.arg,out.arg,"' ",script.file,' 06.',es,'.future_realised.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',spp,'_',es,' -l nodes=1:ppn=12 -l epilogue=/home/jc246980/epilogue/epilogue.sh 06.',es,'.future_realised.sh',sep=''))
}
}