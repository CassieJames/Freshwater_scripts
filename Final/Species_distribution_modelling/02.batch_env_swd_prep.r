#### Batch Script to prepare future environmental data for modelling
#### C. James, L.Hodgson...............9th January 2012

futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"

ESs = list.files(futdir, pattern="RCP") # list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep='')) # get a list of GCMs

sh.dir='/home/jc165798/tmp_pbs/'; setwd(sh.dir)
for (es in ESs) {
for (gcm in GCMs) {
zz = file(paste(es,'.',gcm,'.prep_data.sh',sep=''),'w')
cat('#!/bin/bash\n',file=zz)
cat('cd $PBS_O_WORKDIR\n',file=zz)
cat('source /etc/profile.d/modules.sh\n',file=zz)
cat('module load R-2.15.1\n',file=zz)
cat("R CMD BATCH --no-save --no-restore '--args es=\"",es,"\" gcm=\"",gcm,"\" ' /home/jc165798/SCRIPTS/git_code/NCCARF_freshwater_refugia/SDM/Aus/02.run_env_swd_prep.r ",es,'.',gcm,'.Rout \n',sep='',file=zz) #run the R script in the background
close(zz)

##submit the script
system(paste('qsub -m n -l nodes=1:ppn=5 ',es,'.',gcm,'.prep_data.sh',sep=''))
}
}