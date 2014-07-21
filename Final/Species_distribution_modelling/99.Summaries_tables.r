#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties
##get the delta data in the same format as cassie's delta outputs in preparation for input into her stability summary script
################################################################################

script.file = '/home/jc148322/scripts/NARP_freshwater/SDM/summaries/01.script2run.R'
sh.dir='/home/jc148322/scripts/NARP_freshwater/SDM/summaries/delta.sh/'; dir.create(sh.dir,recursive=T); setwd(sh.dir)

taxa=c('fish','crayfish','frog','turtles')

for (tax in taxa) {
        tax.arg = paste('tax="',tax,'" ',sep='')

        zz = file(paste('01.',tax,'.delta.file.sh',sep=''),'w') ##create the sh file
                cat('#!/bin/sh\n',file=zz)
                cat('cd $PBS_O_WORKDIR\n',file=zz)
                cat("R CMD BATCH --no-save --no-load '--args ",tax.arg,"' ",script.file,' 01.',tax,'.delta.file.Rout \n',sep='',file=zz)
        close(zz)
                        
        #submit the job
        system(paste('qsub -m n -N ',tax,'_delta',' -l nodes=1:ppn=24 01.',tax,'.delta.file.sh',sep=''))
}