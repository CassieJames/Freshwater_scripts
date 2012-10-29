#drafted by Cassie james 
#GNU General Public License .. feel free to use / distribute ... no warranties
####################################################################################
library(SDMTools) #load the necessary library

#### set directories
wd = '/home/jc165798/working/NARP_hydro/'; setwd(wd) #define and set the working directory
out.dir='/home/jc246980/Hydrology.trials/Outputs/Outputs_Futures/'
future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"

ESs=list.files(future.dir, pattern='RCP')
GCMs = list.files(paste(future.dir,pattern=ESs[1],sep=''))
YEARs=seq(2015, 2085,10)

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
dyn.load('/home/jc246980/Freshwater_scripts/Final/Hydrology/Cassie_James/05.Budyko.so') #load the C code

#### Run Budyko code -- staticanalysis

	for(es in ESs) { 
	
		for(gcm in GCMs) {
			
				tmin = read.csv(paste(future.dir,es,"/",gcm,"/","tmn.matrix.csv",sep=''),as.is=TRUE) #read in monthly tmin
				tmax = read.csv(paste(future.dir,es,"/",gcm,"/","tmx.matrix.csv",sep=''),as.is=TRUE) #read in monthly tmax
				pr = read.csv(paste(future.dir,es,"/",gcm,"/","pre.matrix.csv",sep=''),as.is=TRUE) #read in precipitatation			
				
				Eact=matrix(NA,nrow=nrow(tmin),ncol=96)
				tt = expand.grid(sprintf('%02i',1:12),YEARs=YEARs);tt = paste(tt[,2],tt[,1],sep='_'); colnames(Eact) = tt #add the column names
				Qrun=Rnet=Epot=Eact
				
			for(yy in YEARs) {
			
				cois=NULL
                cois = c(cois,grep(yy,colnames(tmin))); tmin_yoi = tmin[,cois]; tmax_yoi=tmax[,cois]; pr_yoi= pr[,cois] #get the columns in the years of interest
                
				###run the analysis and write out data
				tt = .Call('BudykoBucketModelStatic',
					pos$DEM, #dem info
					as.matrix(pr_yoi), # monthly precip
					as.matrix(tmin_yoi), #monthly tmin
					as.matrix(tmax_yoi), #monthly tmax
					pos$PAWHC, #soil water holding capacity
					pos$kRs, #unknown kRs values
					pos$lat / (180/pi), #latitudes in radians
					nrow(pr_yoi), #number of rows that need run
					ncol(pr_yoi) #number of months to run through
				)
			
			Eact[,grep(yy,colnames(Eact))]=tt[[1]]
			Epot[,grep(yy,colnames(Epot))]=tt[[2]]
			Qrun[,grep(yy,colnames(Qrun))]=tt[[3]]
			Rnet[,grep(yy,colnames(Rnet))]=tt[[4]]
			
	
			}		
		
		setwd(out.dir)
		save(Eact, file=paste(out.dir,"Eact/",es,"_",gcm,".Rdata",sep='')) #save the actual evapotranspiration
		save(Epot, file=paste(out.dir,"Epot/",es,"_",gcm,".Rdata",sep='')) #save the potential evapotranspiration
		save(Qrun, file=paste(out.dir,"Qrun/",es,"_",gcm,".Rdata",sep='')) #save the runoff
		save(Rnet, file=paste(out.dir,"Rnet/",es,"_",gcm,".Rdata",sep='')) #save the net radiation
		
		}
	}





