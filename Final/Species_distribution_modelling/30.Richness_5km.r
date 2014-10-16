#Script to determine species richness - adapted from script of Laurens and JV
# C James
# July 2014
#### Determine current and future species richness

taxa = c("fish", "crayfish","frog","turtles"); tax = taxa[4]	
ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[2]
out.dir=paste("/home/jc246980/SDM/Richness/Clip4North/",tax,"/",sep="")
sh.dir='/home/jc246980/SDM/Richness/temp/'; setwd(sh.dir)
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/30.Script2run.r'

	
# determine species richness
	

for (es in ESs) {

real.dir=paste("/home/jc246980/SDM/Realized/",tax,"/Clip4North/",es,"/",sep="") 
cur.dir=paste("/home/jc246980/SDM/Realized/",tax,"/Clip4North/",sep="") 
real.arg = paste('real.dir="',real.dir,'" ',sep='') # working directory argument
cur.arg = paste('cur.dir="',cur.dir,'" ',sep='') # working directory argument
out.arg = paste('out.dir="',out.dir,'" ',sep='') # out directory argument
es.arg = paste('es="',es,'" ',sep='')


zz = file('30.Richness.sh','w') ##create the sh file
cat('#!/bin/sh\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R\n',file=zz)
cat("R CMD BATCH --no-save --no-load '--args ",real.arg,out.arg,es.arg,cur.arg,"' ",script.file,' 30.Richness.Rout \n',sep='',file=zz)
close(zz)

#submit the job
system(paste('qsub -m n -N ',es,' 30.Richness.sh -l pmem=5000mb -l walltime=12:00:00 -l nodes=1:ppn=3 -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))
Sys.sleep(2)

}








