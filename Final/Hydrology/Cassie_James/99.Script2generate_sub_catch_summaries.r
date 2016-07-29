###################################################################################################
### Code to compare accumulated and absolute runoff for river basins across Australia

library(SDMTools) #define the libraries needed

##########set up current climate baseline and attribute identifier
wd = '/home/jc246980/Obsolete/Climate/Baseline_5km/'; setwd(wd)     # define and set working directory
base.asc = read.asc('base.asc')                                            # Import base asc at 5 km resolution
tasc=base.asc                                                                    # Rename baseasc  at 5 km resolution for appending identifier                                                        # 
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution

# Set up unique identifier for 5 km resolution grid

pos$UID = 1:286244    													# append unique identifier

##########import river basin asc and attribute river basin number to position table

river.basin.dir='/home/jc246980/Current projects/Northern Monsoonal/';  setwd(river.basin.dir)
tasc = read.asc("Northsubcatch.asc")
pos$riverbasin  = extract.data(cbind(pos$lon,pos$lat), tasc)  


##########current

load('/home/jc246980/Current projects/Northern Monsoonal/Area_aggregated_by_subcatch_5km.Rdata') #load area relationships table
load("/home/jc246980/Obsolete/Hydrology.trials/Outputs/Output_1976_2005/Qrun.current_5km_means.Rdata")  # Load runoff

Qrun= as.data.frame(Qrun)

tdata=pos 	
tdata$annualsum=rowSums(Qrun[, 1:12])	#Append gridded runoff data																						             
FINAL<- merge(Area_agg, tdata, by='UID')                                          # Merge Area_agg with 5km pos file                            
FINAL$Runoff = (FINAL$AREA/1000000) * FINAL$annual
riverbasin_runoff_final = aggregate(FINAL$Runoff, by = list(FINAL$riverbasin), sum) 


wd="/home/jc246980/Current projects/Northern Monsoonal/"
write.csv(riverbasin_runoff_final,paste(wd,'Runoff_summed_across_riverbasins_means.csv',sep=''))
#create current asc
data.dir="/home/jc246980/Obsolete/Hydrology.trials/Outputs/Output_1976_2005/TomHarwood_data/"
load(paste(data.dir,'Qrun.current_5km_means.Rdata',sep=''))
Qrun.asc=baseasc; Qrun.asc[cbind(pos$row,pos$col)]=Qrun_annuals
write.asc(Qrun.asc, "Runoff_current.asc")

#########Futures
  

datadir="/home/jc246980/Obsolete/Hydrology.trials/Stability/Runoff/"
outdir="/home/jc246980/Obsolete/Hydrology.trials/Niilo_Data/"
ESs=c("RCP45", "RCP85")
YOIS=c(2045, 2085)
outdata = matrix(NA,nrow=15081,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outdata) = tt #add the column names



            for (es in ESs) {
                  tdata=load(paste(datadir,"Data/",es,"_data_runoff.Rdata", sep=''))
                  tdata = get(tdata)
				  tdata=cbind(pos,tdata)
				  tdata<- merge(Area_agg, tdata, by='UID')     
				  
                         for (year in YOIS) {
							 outquant = t(apply(tdata[,grep(year,colnames(tdata))],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
							 outdata[,intersect(grep(year,colnames(outdata)),grep(es,colnames(outdata)))] = outquant[,] #copy out the data
                       }
             }

#save(outdata,file=paste(outdir,"Runoff_data_Nillo.Rdata",sep=''))

outdir="/home/jc246980/Obsolete/Hydrology.trials/Niilo_Data/"; setwd(outdir)
#make asci files of data for Niilo

all.cols=colorRampPalette(c("#A50026","#F46D43","#FEE090", "#FFFFBF","#ABD9E9","#4575B4"))(200)												# append unique identifier
pos=tdata[c(4:7,1)]
ttdata=cbind(tdata[,c(4:7,1)], outdata)
tasc=make.asc(ttdata[,'RCP45_2085_10'])
write.asc(tasc,'RCP45_2085_10.asc' )

#summary of runoff by sub catchment

wd = '/home/jc246980/Obsolete/Climate/Baseline_5km/'; setwd(wd)     # define and set working directory
baseasc = read.asc('base.asc')                                            # Import base asc at 5 km resolution
tasc=baseasc                                                                    # Rename baseasc  at 5 km resolution for appending identifier                                                        # 
pos = read.csv('base.positions.csv',as.is=TRUE)                                 # read in positions on grid at 5 km resolution
pos$UID = 1:286244    													# append unique identifier
SUBCATCH=0:19
##########import river basin 

outtdata = matrix(NA,nrow=20,ncol=3*length(ESs)*length(YOIS)); #define the output matrix
tt = expand.grid(c(10,50,90),YOIS,ESs); tt = paste(tt[,3],tt[,2],tt[,1],sep='_'); colnames(outtdata) = tt #add the column names
rownames(outtdata)=0:19


for (es in ESs) {
                  tdata=load(paste(datadir,"Data/",es,"_data_runoff.Rdata", sep=''))
                  tdata = get(tdata)
				  tdata=cbind(pos,tdata)
				  tdata<- merge(Area_agg, tdata, by='UID')    
				 		
						for (sub in SUBCATCH) {
						temp=tdata[which(tdata$subcatch==sub),]
						ttemp=(temp$AREA/1000000) * temp[,c(8:151)]
						ttemp=as.data.frame(colSums(ttemp))
					
                         for (year in YOIS) {							 
							 							 
							 outquant=quantile(ttemp[grep(year,rownames(ttemp)),], probs = c(0.1, 0.5, 0.9), na.rm = FALSE, names = TRUE, type = 8)
							 outtdata[rownames(outtdata)==sub,intersect(grep(year,colnames(outtdata)),grep(es,colnames(outtdata)))] = outquant #copy out the dat
						}
				}

}
write.csv(outtdata,paste(outdir,'Runoff_summed_across_subcatch.csv',sep=''))