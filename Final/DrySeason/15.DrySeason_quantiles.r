################################################################################
#Cassie james ......................30th July 2012
###now calculate dry season quantiles

library(SDMTools)

out.dir ="/home/jc246980/DrySeason/Futuredat/"
futdir = "/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"	
ESs = list.files(futdir, pattern="RCP") 	
YOIS=seq(2015,2085,10)


vois_data=c("num_month","total_severity","max_clust_length", "fut_clust_severity","fut_month_max_clust")
vois_delta = c("delta_num_month","delta_total_severity","delta_max_clust_length","delta_fut_clust_severity", "delta_month_max_clust")
vois_sd = c("sd_num_month","sd_total_severity","sd_max_clust_length","sd_fut_clust_severity", "sd_month_max_clust")


################################################################################
# calculate quantiles across years and gcms for data

  for (ii in 1:5) { cat(vois_data[ii],'\n')
      outdata = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdata) = tt #add the column names
            for (es in ESs) {
                  tdata=load(paste(out.dir,"Data/",es,"_",vois_data[ii],".Rdata", sep=''))
                  tdata = get(tdata)
                         for (year in YOIS) {
                                     outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
            		                     outdata[,intersect(grep(year,colnames(outdata)),grep(es,colnames(outdata)))] = outquant[,] #copy out the data
                       }
             }
      save(outdata,file=paste(out.dir,"Quantiles_data/",vois_data[ii],"_data.Rdata",sep=''))

  }


################################################################################
# calculate quantiles across years and gcms for deltas

  for (ii in 1:5) { cat(vois_delta[ii],'\n')
      outdelta = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdelta) = tt #add the column names
            for (es in ESs) {
                  tdata=load(paste(out.dir,"Delta/",es,"_",vois_delta[ii],".Rdata", sep=''))
                  tdata = get(tdata)
                         for (year in YOIS) {
                                     outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
            		                     outdelta[,intersect(grep(year,colnames(outdelta)),grep(es,colnames(outdelta)))] = outquant[,] #copy out the data
                       }
             }
      save(outdelta,file=paste(out.dir,"Quantiles_delta/",vois_delta[ii],"_delta.Rdata",sep=''))

  }

# calculate quantiles across years and gcms for sd

  for (ii in 1:5) { cat(vois_sd[ii],'\n')
      outsd = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outsd) = tt #add the column names
            for (es in ESs) {
                  tdata=load(paste(out.dir,"SD/",es,"_",vois_sd[ii],".Rdata", sep=''))
                  tdata = get(tdata)
                         for (year in YOIS) {
                             outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
            		         outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data
                       }
             }
      save(outsd,file=paste(out.dir,"Quantiles_sd/",vois_sd[ii],"_sd.Rdata",sep=''))

  }
  
################################################################################
load(paste(out.dir,"Quantiles_sd/","sd_num_month_sd.Rdata",sep=''))