
####################################################################################
#START R
####################################################################################
dyn.load('/home/jc165798/SCRIPTS/sdmcode/R_development/hydrology/Budyko.so') #load the C code

##### run Budyko code simply providing matrix 12xn where columns represent 12 months -- static analysis
base.asc='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.asc.gz'
pos='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/base.positions.csv'
tmin='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/tmin19402009.csv'
tmax='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/tmax19402009.csv'
pr='/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/rain19402009.csv'
outname='current'

library(SDMTools) #load the necessary library
dyn.load('/home/jc165798/SCRIPTS/sdmcode/R_development/hydrology/Budyko.so') #load the C code

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
yois = 1960:1990#define the years of interest

	
Qrun = NULL
Eact = NULL
Epot = NULL
Rnet = NULL

    for (yy in 1960:1990) { cat(yois[yy],'\n')
        		cois=grep(yy,colnames(pr))
            pr = pr[,cois]
        		cois=grep(yy,colnames(tmin))
            tmin = tmin[,cois]
            cois=grep(yy,colnames(tmax))
            tmax = tmax[,cois]

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
       R_E_act = tt[[1]]
       R_E_pot = tt[[2]]
       R_Q_run = tt[[3]]
       R_Rn = tt[[4]]
    
    Qrun =  cbind(Qrun, R_Q_run)
    Eact =  cbind(Eact, R_E_act)
    Epot =  cbind(Epot, R_E_pot)
    Rnet =  cbind(Rnet, R_Rn)

   }

wd='/home/jc246980/Hydrology.trials/Output_1960_1990';setwd(wd)

save(Eact, file=paste('Eact.',outname,'.Rdata',sep='')) #save the actual evapotranspiration
save(Epot, file=paste('Epot.',outname,'.Rdata',sep='')) #save the potential evapotranspiration
save(Qrun, file=paste('Qrun.',outname,'.Rdata',sep='')) #save the runoff
save(Rnet, file=paste('Rnet.',outname,'.Rdata',sep='')) #save the net radiation

