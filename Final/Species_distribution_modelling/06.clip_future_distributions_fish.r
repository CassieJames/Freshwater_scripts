#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
tax=c('fish','crayfish','turtles','frog')
taxon=tax[1] #change as appropriate
sdm.dir = '/home/jc246980/SDM/'; setwd(sdm.dir)
wd=paste(sdm.dir,'models_',taxon,"/",sep="") ; setwd(wd)
cur.dir=paste('/home/jc246980/SDM/Realized_current/',taxon,'/Clipped2Basin/',sep='')
out.dir=paste('/home/jc246980/SDM/Realised_distributions/',taxon,"/",sep='')

if (taxon==tax[1]) clip.column='Clip2Bio' else clip.column='Clip2RB'

script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/06.script2run_fish.r'

sh.dir='/home/jc246980/SDM/Realised_distributions/temp/'; setwd(sh.dir)

ESs=c('RCP3PD','RCP45','RCP6','RCP85')

for (es in ESs) {
#arguments
es.arg = paste('es="',es,'" ',sep='')
wd.arg = paste('wd="',wd,'" ',sep='')
tax.arg = paste('taxon="',taxon,'" ',sep='')
clip.arg = paste('clip.column="',clip.column,'" ',sep='')
cur.arg=paste('current="',cur.dir,'" ',sep='') 
out.arg=paste('out="',out.dir,'" ',sep='') 

#create sh file
zz = file(paste('05.',es,'.richness.percentiles.sh',sep=''),'w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R-2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",es.arg,tax.arg,wd.arg,clip.arg,"' ",script.file,' 06.',es,'.future_realised.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',tax,'_',es,' -l nodes=1:ppn=12 06.',es,'.future_realised.sh',sep=''))
}
