######################################################################W############################
#Script to interegate current and future climates for RAMSAR listed wetlands across Australia
#C. James..........................................7th November 2012

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
out.dir='/home/jc246980/RAMSAR/Output/'

###Load climate stability data

stability.dir ="/home/jc165798/working/NARP_hydro/stability/OZ_5km/percent_sd/"

###Load RAMSAR data

RAMasc = read.asc("/home/jc246980/RAMSAR/ramsar_wetlands_for_download.asc") # load ramsar  asc
RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')
pos$RAMSAR  = extract.data(cbind(pos$lon,pos$lat), RAMasc)    

####Run through stability for mean temperature

out = NULL # create an empty dataframe
  
  
    for (es in ESs) { cat(es,'\n') #cycle through each of the directories
       for (gcm in GCMs) {
          temp = load(paste(stability.dir,es,'_',gcm,'_',"tmp.Rdata", sep=""))#read in data
          RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector
          for (ram in RAMSARS) {
            tdata = futsd[which(pos$RAMSAR==ram),] #get the data only for the ibra of interest
            ###summarize for the ibra region
            years = as.numeric(unique(substr(gsub('tdata','',colnames(tdata)[-c(1:2)]),4,7)))
         for (year in years) {
           ttemp = as.matrix(tdata[,grep(year,colnames(tdata))]) # matches years to column names in the temp file creates second matrix
           out2 = colMeans(ttemp)
           out = rbind(out, data.frame(ES=es,GCM=gcm,YEAR=year,RAMSARS=ram,m01=out2[1],m02=out2[2],m03=out2[3],
           m04=out2[4],m05=out2[5],m06=out2[6],m07=out2[7],m08=out2[8],m09=out2[9],m10=out2[10],m11=out2[11],m12=out2[12]))
  }
  }
  }
  }
  
out.dir='/home/jc246980/RAMSAR/Output/'
write.csv(out,paste(out.dir,"tmp_stability_ramsar.csv",sep=''),row.names=F)	 