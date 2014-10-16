####Script to grid out all freshwater species distributions

#### Set basic parameters

tax=c('fish','crayfish','turtles','frog')
taxon=tax[1] #change as appropriate

sdm.dir='/home/jc246980/SDM/'

script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/08.script2run.r'
sh.dir='/home/jc246980/SDM/Realized_gridded/temp/'; setwd(sh.dir)


ESs=c('RCP3PD','RCP45','RCP6','RCP85'); es=ESs[1]

for (es in ESs) {

cur.dir=paste('/home/jc246980/SDM/Realized/',taxon,'/Clip4North/',es,"/",sep='')
species=list.files(cur.dir, pattern="fut.real.mat")
species=gsub(".fut.real.mat.Rdata", "", species)


	for (spp in species) {cat (spp,'\n')

	out.dir = paste("/home/jc246980/SDM/Realized_gridded/",taxon,"/",es,"/", spp, sep=""); dir.create(out.dir)
	#arguments
	es.arg = paste('es="',es,'" ',sep='')
	tax.arg = paste('taxon="',taxon,'" ',sep='')
	out.arg=paste('out.dir="',out.dir,'" ',sep='') 
	cur.arg=paste('cur.dir="',cur.dir,'" ',sep='') 
	spp.arg = paste('spp="',spp,'" ',sep='') # species argument

	#create sh file
	zz = file(paste('08.',es,'.gridded.sh',sep=''),'w') ##create the sh file
	cat('#!/bin/sh\n',file=zz)
	cat('cd $PBS_O_WORKDIR\n',file=zz)
	cat('source /etc/profile.d/modules.sh\n',file=zz)
	cat('module load R\n',file=zz)
	cat("R CMD BATCH --no-save --no-load '--args ",es.arg,tax.arg,out.arg,cur.arg,out.arg,spp.arg,"' ",script.file,' 08.',es,'.gridded.Rout \n',sep='',file=zz)
	close(zz)

	#submit the job
	system(paste('qsub -m n -N ',spp,'_',es,' 08.',es,'.gridded.sh -l pmem=8000mb -l walltime=48:00:00 -l nodes=1:ppn=3  -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))



	
	}
}