
data.dir='/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1976to2005/';setwd(data.dir)
out.dir='/home/jc246980/Hydrology.trials/Accumulated_reach/Output_futures/Qrun_accumulated2reach_1976to2005/'

files=list.files()
files=grep('RCP',files,value=T)

sh.dir='/home/jc148322/scripts/NARP_freshwater/Hydrology/accumulate_futures.sh/'; dir.create(sh.dir,recursive=T); setwd(sh.dir)

for (tfile in files){ #tfile=files[3]
	es=strsplit(tfile,'_')[[1]][1]
	gcm=gsub('.Rdata','',strsplit(tfile,'_')[[1]][2])

	zz = file(paste('50.',es,'.',gcm,'.accumulate.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat('source /etc/profile.d/modules.sh\n',file=zz)
		 cat('module load R-2.15.1\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args tfile=\"",tfile,"\" data.dir=\"",data.dir,"\" out.dir=\"",out.dir,"\" ' ~/scripts/NARP_freshwater/Hydrology/50.run.accumulate.future.r 50.",es,'.',gcm,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n -l nodes=2 50.',es,'.',gcm,'.accumulate.sh',sep=''))


}