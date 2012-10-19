################################################################################
#Cassie james ......................30th July 2012
###now calculate dry season quantiles

library(SDMTools)

out.dir ="/home/jc246980/DrySeason/Futuredat/"
#load deltas and standard deviations

future.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/pre/'
files=list.files(future.dir, pattern='RCP')
ESs = unlist(strsplit(files,"_")); ESs = unique(ESs[seq(1,length(ESs),2)])#list the emission scenarios
YOIS=seq(2015,2085,10)


vois_files=c("sd_num_month.Rdata","sd_total_severity.Rdata","sd_max_clust_length.Rdata", "sd_fut_clust_severity.Rdata","sd_month_max_clust.Rdata")
vois_delta = c("delta.num.month","delta.total.severity","delta.max.clust.length","delta.clust.severity", "delta.month.max.clust")
vois_sd = c("sd.num.month","sd.total.severity","sd.max.clust.length","sd.clust.severity", "sd.month.max.clust")


################################################################################
# calculate quantiles across years and gcms for deltas

  for (ii in 1:5) { cat(vois_sd[ii],'\n')
      outsd = matrix(NA,nrow=286244,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
  	  tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outsd) = tt #add the column names
            for (es in ESs) {
                  load(paste(out.dir,"SD/",es,"_",vois_files[ii], sep=''))
                  tdata = get(vois_sd[ii])
                         for (year in YOIS) {
                                     outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
            		                     outsd[,intersect(grep(year,colnames(outsd)),grep(es,colnames(outsd)))] = outquant[,] #copy out the data
                       }
             }
      save(outsd,file=paste(out.dir,"Quantiles_sd/",vois_sd[ii],"_SD.Rdata",sep=''))

  }

  
################################################################################
