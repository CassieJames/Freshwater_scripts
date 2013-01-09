################################################################################
# Batch script to aggregate dry season severity variables generated at 5km resolution onto Janets reaches for futures
# C. James 20th November 2012

###Set directories

futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/'	
data.dir="/home/jc246980/DrySeason/Futuredat/Data/"
out.dir="/home/jc246980/DrySeason/DrySeason_reach/"
sh.dir ="/home/jc246980/Freshwater_scripts/sh/Aggregate_dryseason/"; setwd(sh.dir)


ESs = list.files(futdir, pattern="RCP") 										# list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep=''))									# get a list of GCMs
VOIS=c("num_month", "total_severity", "max_clust_length","fut_clust_severity", "fut_month_max_clust")

	for(voi in VOIS) {  	
		
		for(es in ESs) { 											
					
			for(gcm in GCMs) {
					
					zz = file(paste(es,'.',gcm,'.',voi,'.aggregate.sh',sep=''),'w')
					cat('#!/bin/bash\n',file=zz)
                    cat('cd $PBS_O_WORKDIR\n',file=zz)
                    cat("R CMD BATCH --no-save --no-restore '--args es=\"",es,"\" voi=\"",voi,"\" gcm=\"",gcm,"\" data.dir=\"",data.dir,"\" out.dir=\"",out.dir,"\" ' ~/Freshwater_scripts/Final/DrySeason/40.Run.DrySeason_aggregated2reaches_future.r ",es,'.',gcm,'.',voi,'.Rout \n',sep='',file=zz) #run the R script in the background
                    close(zz) 

                    ##submit the script
                    system(paste('qsub -l nodes=1:ppn=3 ',es,'.',gcm,'.',voi,'.aggregate.sh',sep=''))

			
			}

		}
		
	}	      