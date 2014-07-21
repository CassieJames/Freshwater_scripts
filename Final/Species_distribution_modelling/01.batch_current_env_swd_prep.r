#### Batch Script to prepare current environmental data for modelling
#### C. James, L.Hodgson...............9th January 2012


sh.dir='/home/jc246980/SDM/temp/'; setwd(sh.dir)

zz = file(paste('current.prep_data.sh',sep=''),'w')
cat('#!/bin/bash\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R-2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-restore '--args ' /home/jc246980/Freshwater_scripts/Final/Species_distribution_modelling/01.current_env_swd.r ",'Current.Rout \n',sep='',file=zz) #run the R script in the background
close(zz)

##submit the script
system(paste('qsub -m n -l nodes=1:ppn=5 ','current.prep_data.sh',sep=''))
