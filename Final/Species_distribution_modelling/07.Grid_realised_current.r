####Script to grid out all freshwater species distributions

#### Set basic parameters

tax=c('fish','crayfish','turtles','frog')
taxon=tax[3] #change as appropriate

sdm.dir='/home/jc246980/SDM/'
cur.dir=paste('/home/jc246980/SDM/Realized/',taxon,'/Clip4North/',sep='')
script.file = '/home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/07.script2run.r'
sh.dir='/home/jc246980/SDM/Realized/temp/'; setwd(sh.dir)
out.dir =paste("/home/jc246980/SDM/Realized_gridded/",taxon,"/", sep="")
species=list.files(cur.dir, pattern="cur.real.mat")
species=gsub(".cur.real.mat.Rdata", "", species)

ESs=c('RCP3PD','RCP45','RCP6','RCP85'); es=ESs[2]


#for (es in ESs) {

	for (spp in species) {cat (spp,'\n')

	#arguments
	es.arg = paste('es="',es,'" ',sep='')
	tax.arg = paste('taxon="',taxon,'" ',sep='')
	cur.arg=paste('cur.dir="',cur.dir,'" ',sep='') 
	out.arg=paste('out.dir="',out.dir,'" ',sep='') 
	spp.arg = paste('spp="',spp,'" ',sep='') # species argument

	#create sh file
	zz = file(paste('07.current.gridded.sh',sep=''),'w') ##create the sh file
	cat('#!/bin/sh\n',file=zz)
	cat('cd $PBS_O_WORKDIR\n',file=zz)
	cat('source /etc/profile.d/modules.sh\n',file=zz)
	cat('module load R\n',file=zz)
	cat("R CMD BATCH --no-save --no-load '--args ",es.arg,tax.arg,cur.arg,out.arg,spp.arg,"' ",script.file,' 07.current.gridded.Rout \n',sep='',file=zz)
	close(zz)

	#submit the job
	system(paste('qsub -m n -N ',spp,'_',es,' 07.current.gridded.sh -l pmem=2000mb -l walltime=00:12:00 -l nodes=1:ppn=3  -l epilogue=/home/jc246980/epilogue/epilogue.sh',sep=''))


	}
#}