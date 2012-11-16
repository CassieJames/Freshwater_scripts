#drafted by Cassie james 
#GNU General Public License .. feel free to use / distribute ... no warranties
####################################################################################
library(SDMTools) #load the necessary library

#### set directories
wd = '/home/jc165798/working/NARP_hydro/'; setwd(wd) #define and set the working directory
out.dir='/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/'
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(futures.dir,pattern=ESs[1],sep=''))

#### read in inputs
base.asc='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.asc.gz'
pos='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.positions.csv'
base.asc = read.asc.gz(base.asc) #read in the base asc file
pos = read.csv(pos,as.is=TRUE)
pos$PAWHC = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/PAWHC_5km.asc')) #append data to pos
pos$PAWHC[which(is.na(pos$PAWHC))] = mean(pos$PAWHC,na.rm=TRUE) #set missing data to mean values
pos$kRs = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/kRs_5km.asc'))
pos$kRs[which(is.na(pos$kRs))] = 0.16 #set missing data to 0.16
pos$DEM = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/DEM_5km.asc'))

#### load Budyko code 
dyn.load('/home/jc246980/Scripts/Final/Hydrology/Cassie_James/05.Budyko.so') #load the C code

#### Run Budyko code -- dynamic analysis

	for(es in ESs) { 
	
		for(gcm in GCMs) {
			
			tmin = read.csv(paste(future.dir,es,"/",gcm,"/","tmn.matrix.csv",sep=''),as.is=TRUE) #read in monthly tmin
			tmax = read.csv(paste(future.dir,es,"/",gcm,"/","tmx.matrix.csv",sep=''),as.is=TRUE) #read in monthly tmax
			pr = read.csv(paste(future.dir,es,"/",gcm,"/","pre.matrix.csv",sep=''),as.is=TRUE) #read in precipitatation

			tmin = tmin[,c(-1,-2)] # remove longs and lats from the start of the data file
			tmax= tmax[,c(-1,-2)] # remove longs and lats from the start of the data file
			tmin = tmax[,c(-1,-2)] # remove longs and lats from the start of the data file

			###run the analysis and write out data
			tt = .Call('BudykoBucketModelDynamic',
				pos$DEM, #dem info
				as.matrix(pr), # monthly precip
				as.matrix(tmin), #monthly tmin
				as.matrix(tmax), #monthly tmax
				pos$PAWHC, #soil water holding capacity
				pos$kRs, #unknown kRs values
				pos$lat / (180/pi), #latitudes in radians
				nrow(pr), #number of rows that need run
				ncol(pr) #number of months to run through
			)
		
			setwd(out.dir)

			Eact = tt[[1]]; save(Eact, file=paste(out.dir,"Eact/",es,"_",gcm,".Rdata",sep='')) #save the actual evapotranspiration
			Epot = tt[[2]]; save(Epot, file=paste(out.dir,"Epot/",es,"_",gcm,".Rdata",sep='')) #save the potential evapotranspiration
			Qrun = tt[[3]]; save(Qrun, file=paste(out.dir,"Qrun/",es,"_",gcm,".Rdata",sep='')) #save the runoff
			Rnet = tt[[4]]; save(Rnet, file=paste(out.dir,"Rnet/",es,"_",gcm,".Rdata",sep='')) #save the net radiation
				
		
		
		}
	}





