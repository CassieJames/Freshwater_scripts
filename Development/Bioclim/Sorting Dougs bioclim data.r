library(SDMTools)
library(raster)

gisfuture.dir = "/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/"


vois = c('bioclim_02','bioclim_11','bioclim_12','bioclim_18')
YEAR=seq(2015, 2085, 10)

futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it
ESs = list.files(futdir, pattern="RCP") #list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep='')) #get a list of GCMs


out.dir = "/home/jc246980/WaterHole_Persistence/Future_data/"; setwd(out.dir) #define directory

for (voi in vois) { cat(voi,'\n') #cycle through each variable
	for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse
      for(year in YEAR) { cat(year,'\n')

          wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
          pos = read.csv('base.positions.csv',as.is=TRUE)


          for (gcm in GCMs) { cat(gcm,'\n') #cycle through each GCM & prepare all data for reuse
                tasc = read.asc.gz(paste(gisfuture.dir,es,"_",gcm,"_",year,"/",voi,".asc.gz",sep=''))
                tdata = as.data.frame(which(is.finite(tasc),arr.ind=T))
                tdata[,gcm]= tasc[cbind(pos$row,pos$col)]
                pos=cbind(pos,tdata[,gcm])
          }
            colnames(pos) = c('lat','lon','row','col',GCMs)
            save(pos,file=paste(out.dir,voi,"/",es,"_",year,"_",voi,".Rdata",sep=''))
      }
   }
}
