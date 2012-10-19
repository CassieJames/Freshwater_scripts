################################################################################
# code to determine differences between quantiles and individual GCMs

library(SDMTools) #load the necessary library

gisfuture.dir = "/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/"
out.dir = "/home/jc246980/WaterHole_Persistence/Future_data/"; setwd(out.dir) #define directory
futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)


ESs = list.files(futdir, pattern="RCP") #list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep='')) #get a list of GCMs
YEAR= seq(2015, 2085,10)
vois = c('bioclim_12','bioclim_18')    # rem to add bioclim_02 back if script rerun

outdiffs = matrix(NA,nrow=18,ncol=3*length(ESs)*length(YEAR))#define the output matrix
tt = expand.grid(c(10,50,90),YEAR,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdiffs) = tt #add the column names
rownames(outdiffs)=GCMs

    for (voi in vois) {
              load(paste(out.dir,voi,"/","outquants.Rdata", sep=''))

          for(ii in 1:ncol(out)) {
                 es = strsplit(colnames(out)[ii],"_")[[1]][1]  # gives the es
                 year = strsplit(colnames(out)[ii],"_")[[1]][2]  # gives the year
                 qtiles = strsplit(colnames(out)[ii],"_")[[1]][3]

              for(gcm in GCMs) { cat(gcm,'\n')

                 tasc = read.asc.gz(paste(gisfuture.dir,es,"_",gcm,"_",year,"/",voi,".asc.gz",sep=''))
                 tdata = as.data.frame(which(is.finite(tasc),arr.ind=T))
                 tdata[,gcm]= tasc[cbind(pos$row,pos$col)]
                 ttemp = sum(abs(out[,ii]/tdata[,gcm]))
                 outdiffs[grep(gcm,rownames(outdiffs)),intersect(intersect(grep(es,colnames(outdiffs)),grep(year,colnames(outdiffs))),grep(qtiles,colnames(outdiffs)))] = ttemp #copy out the data

              }

          }

       save(outdiffs,file=paste(out.dir,voi,"/","Outdiffs.Rdata",sep=''))
       write.csv(outdiffs,paste(out.dir,voi,"/","Outdiffs.csv", sep = ''))
    }
    
