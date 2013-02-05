################################################################################
### Script to calculate changes in runoff - remember need to use static analysis of 30 years (Toms data) for current!
### Cassie james ......................10th January 2013


library(SDMTools)

data.dir ="/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/Qrun/"
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
ESs = list.files(futdir, pattern="RCP") 	
GCMs = list.files(paste(futdir,ESs[1],sep=''))	
YOIS=seq(2015,2085,10)
out.dir="/home/jc246980/Hydrology.trials/Stability/Runoff/"
future.dir="/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/Qrun/" # location of future runoff
wd ="/home/jc246980/Hydrology.trials/Outputs/Output_1976_2005/TomHarwood_data/" # location of current runoff
load(paste(wd, "Qrun.current_5km_means.Rdata", sep=''))

Current=as.matrix(rowSums(Qrun))

	for (es in ESs){
		  delta.runoff= matrix(NA,nrow=nrow(Qrun),ncol=144)
		  tt = expand.grid(GCMs,YOIS); tt = paste(tt[,1],tt[,2],sep='_'); colnames(delta.runoff) = tt #add the column names
		  sd.runoff=data.runoff=delta.runoff
		  
		 for (gcm in GCMs){   cat(gcm,'\n')
				   load(paste(future.dir,es,'_',gcm,'.Rdata', sep=''))
					
					for (yy in YOIS){   cat(yy,'\n')
					
					cois= grep(yy,colnames(Qrun))# Columns of interest 
					future=as.matrix(rowSums(Qrun[,cois])) # Annual total
					
					data.runoff[,intersect(grep(yy,colnames(data.runoff)),grep(gcm,colnames(data.runoff)))]=future
					delta.runoff[,intersect(grep(yy,colnames(delta.runoff)),grep(gcm,colnames(delta.runoff)))]= future/(Current+0.000001)
					#sd.runoff[,intersect(grep(yy,colnames(sd.runoff)),grep(gcm,colnames(sd.runoff)))]=???

					}
		}
	
	    save(delta.runoff,file=paste(out.dir,"Delta/",es,"_delta_runoff.Rdata",sep=''))
		save(data.runoff,file=paste(out.dir,"Data/",es,"_data_runoff.Rdata",sep=''))
		#save(sd.runoff,file=paste(out.dir,"SD/",es,"_sd_runoff.Rdata",sep=''))
	}



################################################################################
# calculate quantiles across years and gcms for data


      outdata = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdata) = tt #add the column names
            for (es in ESs) {
                  tdata=load(paste(out.dir,"Data/",es,"_data_runoff.Rdata", sep=''))
                  tdata = get(tdata)
                         for (year in YOIS) {
							 outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
							 outdata[,intersect(grep(year,colnames(outdata)),grep(es,colnames(outdata)))] = outquant[,] #copy out the data
                       }
             }
      save(outdata,file=paste(out.dir,"Quantiles_data/","Runoff_data.Rdata",sep=''))

  
################################################################################
# calculate quantiles across years and gcms for deltas

      outdelta = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt #add the column names
            for (es in ESs) {
                  tdata=load(paste(out.dir,"Delta/",es,"_delta_runoff.Rdata", sep=''))
                  tdata = get(tdata)
                         for (year in YOIS) {
								 outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
								 outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
                       }
             }
      save(outdelta,file=paste(out.dir,"Quantiles_delta/","Runoff_delta.Rdata",sep=''))

################################################################################



