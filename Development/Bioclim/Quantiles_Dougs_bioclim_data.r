library(SDMTools)
library(stringr)

vois = c('bioclim_11','bioclim_12','bioclim_18')
out.dir = "/home/jc246980/WaterHole_Persistence/Future_data/"; setwd(out.dir) #define directory

YEAR=seq(2015, 2085, 10)
futdir = '/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/' #define the directory with all the future data in it
ESs = list.files(futdir, pattern="RCP") #list the emission scenarios

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
     
out.dir = "/home/jc246980/WaterHole_Persistence/Future_data/bioclim_12/"; setwd(out.dir) #define directory
load(paste(out.dir,'outquants.Rdata',sep=''))
load(paste(out.dir,'RCP85_2085_bioclim_12.Rdata',sep=''))
