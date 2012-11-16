#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

####################################################################################
#START R
####################################################################################

##### run Budyko code simply providing matrix 12xn where columns represent 12 months -- dynamic analysis
base.asc='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.asc.gz'
pos='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.positions.csv'
tmin='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/tmin19402009.csv'
tmax='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/tmax19402009.csv'
pr='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/rain19402009.csv'
outname='current'

library(SDMTools) #load the necessary library
dyn.load('/home/jc246980/Freshwater_scripts/Final/Hydrology/Cassie_James/05.Budyko.so') #load the C code

###set directories
wd = '/home/jc165798/working/NARP_hydro/'; setwd(wd) #deifne and set the working directory

####read in all necessary inputs
base.asc = read.asc.gz(base.asc) #read in the base asc file
pos = read.csv(pos,as.is=TRUE)
pos$PAWHC = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/PAWHC_5km.asc')) #append data to pos
pos$PAWHC[which(is.na(pos$PAWHC))] = mean(pos$PAWHC,na.rm=TRUE) #set missing data to mean values
pos$kRs = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/kRs_5km.asc'))
pos$kRs[which(is.na(pos$kRs))] = 0.16 #set missing data to 0.16
pos$DEM = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/DEM_5km.asc'))

if (outname=='current') {
	tmin = read.csv(tmin,as.is=TRUE)[,-c(1:2)] #read in monthly tmin
	tmax = read.csv(tmax,as.is=TRUE)[,-c(1:2)] #read in monthly tmax
	pr = read.csv(pr,as.is=TRUE)[,-c(1:2)] #read in precipitatation
} else {
	tmin = read.csv(tmin,as.is=TRUE) #read in monthly tmin
	tmax = read.csv(tmax,as.is=TRUE) #read in monthly tmax
	pr = read.csv(pr,as.is=TRUE) #read in precipitatation
}

#trim up the data to years of interest
yois = 1976:2005
cois = NULL; for (yy in yois) cois = c(cois,grep(yy,colnames(pr)))
tmin = tmin[,cois]; tmax = tmax[,cois]; pr = pr[cois]

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

wd='/home/jc246980/Hydrology.trials/Output_1976_2005';setwd(wd)

Eact = tt[[1]]; save(Eact, file=paste(wd,'/E_act_30yearagg_dynamic',sep='')) #save the actual evapotranspiration
Epot = tt[[2]]; save(Epot, file=paste(wd,'/E_pot_30yearagg_dynamic',sep='')) #save the potential evapotranspiration
Qrun = tt[[3]]; save(Qrun, file=paste(wd,'/Q_run_30yearagg_dynamic',sep='')) #save the runoff
Rnet = tt[[4]]; save(Rnet, file=paste(wd,'/R_net_30yearagg_dynamic',sep='')) #save the net radiation



