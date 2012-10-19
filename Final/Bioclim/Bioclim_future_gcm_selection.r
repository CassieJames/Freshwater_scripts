################################################################################
# Script to determine GCMs that are least different to 10, 50 and 90th percentiles of future projections
# C James 11th October 2012

#load necessary libraries
library(SDMTools)
library(raster)
library(stringr)

#Set directories
gisfuture.dir = "/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/"
out.dir = "/home/jc246980/WaterHole_Persistence/Future_data/"; setwd(out.dir) #define directory
futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it

#
ESs = list.files(futdir, pattern="RCP") #list the emission scenarios
GCMs = list.files(paste(futdir,ESs[1],sep='')) #get a list of GCMs
vois = c('bioclim_02','bioclim_11','bioclim_12','bioclim_18')
YEAR=seq(2015, 2085, 10)

# Code to prepare bioclim asciis

for (voi in vois) { cat(voi,'\n') #cycle through each variable

  for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse

      for(year in YEAR) { cat(year,'\n')
          pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)
          
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

# Code to determine quantiles of GCMs for future predictions per ES and year

   for (voi in vois)  {

          tdatafiles = list.files(paste(out.dir,voi, sep=''), pattern="bioclim")

          wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)
          pos = read.csv('base.positions.csv',as.is=TRUE)

          out = matrix(NA,nrow=nrow(pos),ncol=3*length(ESs)*length(YEAR)); #define the output matrix
          tt = expand.grid(c(10,50,90),YEAR,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(out) = tt #add the column names
          cois=5:22

          for(ii in 1:length(tdatafiles)) { cat(ii,'\n') #cycle through each emission & prepare all data for reuse
                load(paste(out.dir,voi,'/',tdatafiles[ii],sep=''))
          			outquant = t(apply(pos[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
                out[,intersect(grep(strsplit(tdatafiles[ii],"_")[[1]][2],colnames(out)),grep(strsplit(tdatafiles[ii],"_")[[1]][1],colnames(out)))] = outquant[,] #copy out the data
          }


          save(out,file=paste(out.dir,voi,'/outquants.Rdata',sep=''))
     }

 # Code to determine sum of absolute or proportional differences between quantiles and GCMs
 
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
    
################################################################################
#Code to subset data for doug

futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it
gisfuture.dir = "/home/jc165798/Climate/CIAS/Australia/5km/bioclim_asc/"
doug.dir ="/home/jc246980/WaterHole_Persistence/Future_data/Future_data_Doug/"

GCMoi=c("inm-cm30", "mri-cgcm232a", "csiro-mk30", "ncar-pcm1", "ccsr-miroc32med", "ipsl-cm4", "mpi-echam5", "cccma-cgcm31", "iap-fgoals10g")
ESs = list.files(futdir, pattern="RCP") #list the emission scenarios
YEAR=seq(2015, 2085, 10)
vois = c('bioclim_02','bioclim_11', 'bioclim_12', 'bioclim_18')

  for(es in ESs) { cat(es,'\n') #cycle through each emission & prepare all data for reuse

      for(year in YEAR) { cat(year,'\n')
             
          for (gcm in GCMoi) { cat(gcm,'\n') #cycle through each GCM & prepare all data for reuse
                          
            for(voi in vois) { cat(voi,'\n') 
                tasc = read.asc.gz(paste(gisfuture.dir,es,"_",gcm,"_",year,"/",voi,".asc.gz",sep=''))
                
				write.asc.gz(tasc,paste(doug.dir,es,"_",gcm,"_", year, "_",voi,sep=''))


          
          }
      }
   }
}