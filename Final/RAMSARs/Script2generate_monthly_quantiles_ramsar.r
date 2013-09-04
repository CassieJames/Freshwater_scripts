######################################################################W############################
#Script to integrate current and future climates for RAMSAR listed wetlands across Australia and calculate quantiles
#C. James..........................................7th November 2012

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed
source('/home/jc148322/scripts/libraries/cool_functions.r')

###Set up base files
wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/'; setwd(wd) #define and set the working directory
baseasc = read.asc.gz('base.asc.gz');
pos = as.data.frame(which(is.finite(baseasc),arr.ind=T))
pos$lat = getXYcoords(baseasc)$y[pos$col]
pos$lon = getXYcoords(baseasc)$x[pos$row] #append the lat lon
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
years=seq(2015, 2085,10)
out.dir='/home/jc246980/RAMSAR/Output/RAMSAR_site_data/tmp/'

###Load climate stability data

stability.dir ="/home/jc165798/working/NARP_hydro/stability/OZ_5km/percent_sd/"

###Load RAMSAR data

RAMasc = read.asc("/home/jc246980/RAMSAR/RAMSAR.asc") # load ramsar  asc
RAMinfo = read.dbf('/home/jc246980/RAMSAR/RAMSAR_info.dbf')
pos$RAMSAR  = extract.data(cbind(pos$lon,pos$lat), RAMasc)    
RAMSARS = unique(na.omit(pos$RAMSAR)) # create RAMSAR vector

###Run through stability for mean temperature

out = NULL # create an empty dataframe
mm=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec') # set up column headings
tt = expand.grid(mm,GCMs,ESs);tt = paste(tt[,3],tt[,2],tt[,1],sep='_')

  
	for (ram in RAMSARS) { cat(ram,'\n')
		
		for (year in years) { cat(year,'\n') 
			
			out=NULL
			
			for (es in ESs) { cat(es,'\n') #cycle through each of the directories
			   
			   for (gcm in GCMs) { cat(gcm,'\n')
				   
				   load(paste(stability.dir,es,'_',gcm,'_',"tmp.Rdata", sep=""))#read in data
				   tdata = futsd[which(pos$RAMSAR==ram),] #get the data only for the ramsar of interest
				   ttemp = as.matrix(tdata[,grep(year,colnames(tdata))]) # matches years to column names in the temp file creates second matrix 
				   out=cbind(out, ttemp)


				}
			}
			colnames(out)=tt
			write.csv(out,paste(out.dir,ram,"_",year,'.csv',sep=''))
		}
	}
	  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



  