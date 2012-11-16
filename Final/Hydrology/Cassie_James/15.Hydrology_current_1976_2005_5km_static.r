####################################################################################
##### run Budyko code for 30 year means-- static analysis at 5km using 5km PAWHC - for Tom Harwood 28th September 2012

base.asc='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz'
pos='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv'
tmin='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/monthly.tmin.csv'
tmax='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/monthly.tmax.csv'
pr='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/monthly.pr.csv'
outname='current_5km_means'

library(SDMTools) #load the necessary library
dyn.load('/home/jc246980/Freshwater_scripts/Final/Hydrology/Cassie_James/05.Budyko.so') #load the C code

###set directories
wd = '/home/jc165798/working/NARP_hydro/'; setwd(wd) #define and set the working directory

####read in all necessary inputs
base.asc = read.asc.gz(base.asc) #read in the base asc file
pos = read.csv(pos,as.is=TRUE)
pos$PAWHC = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/PAWHC_5km.asc')) #append data to pos
pos$PAWHC[which(is.na(pos$PAWHC))] = mean(pos$PAWHC,na.rm=TRUE) #set missing data to mean values
pos$kRs = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/kRs_5km.asc'))
pos$kRs[which(is.na(pos$kRs))] = 0.16 #set missing data to 0.16
pos$DEM = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/DEM_5km.asc'))

if (outname=='current_5km_means') {
	tmin = read.csv(tmin,as.is=TRUE)[,-c(1:2)] #read in monthly tmin
	tmax = read.csv(tmax,as.is=TRUE)[,-c(1:2)] #read in monthly tmax
	pr = read.csv(pr,as.is=TRUE)[,-c(1:2)] #read in precipitatation
} else {
	tmin = read.csv(tmin,as.is=TRUE) #read in monthly tmin
	tmax = read.csv(tmax,as.is=TRUE) #read in monthly tmax
	pr = read.csv(pr,as.is=TRUE) #read in precipitatation
}

###run the analysis and write out data
tt = .Call('BudykoBucketModelStatic',
	pos$DEM, #dem info
	as.matrix(pr), # monthly precip
	as.matrix(tmin), #monthly tmin
	as.matrix(tmax), #monthly tmax
	pos$PAWHC, #soil water holding capacity
	pos$kRs, #unknown kRs values 
	pos$lat / (180/pi), #latitudes in radians
	nrow(pr) #number of rows that need run
)


wd='/home/jc246980/Hydrology.trials/Output_1976_2005/TomHarwood_data/';setwd(wd)

Eact = tt[[1]]; save(Eact, file=paste('Eact.',outname,'.Rdata',sep='')) #save the actual evapotranspiration
Epot = tt[[2]]; save(Epot, file=paste('Epot.',outname,'.Rdata',sep='')) #save the potential evapotranspiration
Qrun = tt[[3]]; save(Qrun, file=paste('Qrun.',outname,'.Rdata',sep='')) #save the runoff
Rnet = tt[[4]]; save(Rnet, file=paste('Rnet.',outname,'.Rdata',sep='')) #save the net radiation

library(SDMTools) #load the necessary library
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

wd="/home/jc246980/Hydrology.trials/Output_1976_2005/TomHarwood_data/"; setwd(wd)

 #read in the base asc file

load(paste(data.dir,'/Eact.current_5km_means.Rdata',sep=''))
load(paste(data.dir,'/Epot.current_5km_means.Rdata',sep=''))
load(paste(data.dir,'/Qrun.current_5km_means.Rdata',sep=''))
load(paste(data.dir,'/Rnet.current_5km_means.Rdata',sep=''))

vois=c("Eact", "Epot", "Qrun", "Rnet")


  for(voi in vois) {
        for (ii in 1:12) { cat(ii, '\n')
            data=get(voi)
            tasc=base.asc
            tasc[cbind(pos$row,pos$col)]=data[,ii]
            write.asc(tasc, file=paste(voi,ii,".asc",sep=''))
        }
  }