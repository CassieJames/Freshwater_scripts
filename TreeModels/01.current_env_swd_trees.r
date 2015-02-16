###################################################################################################
### Script to setup current files for MAXENT run trials of Trees and shrub distributions

library(SDMTools)
library(maptools) 
library(igraph)
library(parallel) #load the necessary libraries#define the libraries needed
source('/home/jc148322/scripts/libraries/cool_functions.r')
### set up classes in case they are useful

terrain = read.dbf('/home/jc246980/Janet_Stein_data/Terrain.dbf')


cois=c('SEGMENTNO', 'VALLEYSLOP', 'CATSLOPE','D2OUTLET', "STRAHLER")
terrain_sub=terrain[,cois]
colnames(terrain_sub)=c("SegmentNo", "Segslope", "Catslope", "d2outlet", "STRAHLER")
bioclim=read.csv("/home/jc246980/Obsolete/Climate/5km/Future/Bioclim_reach/Current_bioclim_agg2reach_1976to2005.csv") # read in bioclim variables


out = bioclim; out$lat = out$lon = out$SegmentNo; out = out[,c('SegmentNo','lat','lon',colnames(bioclim)[-1])] #define the output data replicating segment number as lat and lon

species.data=read.csv("/home/jc246980/Riparian_Models/Data/Melaleucabracteata_simp.csv")





### Create current environmental data file
for(voi in VOIS) { cat(voi,'\n')
tdata=read.csv(paste(dryseason.dir,"Current_",voi,".csv", sep='')) # load data for each varable
if (voi==VOIS[1]) Enviro_dat=tdata else Enviro_dat=merge(Enviro_dat,tdata)
}

out = merge(out,Enviro_dat); out = merge(out,HYDROLOGY) #fully define the output
out=merge(out,terrain_sub)

current = out; save(current,file='/home/jc246980/SDM/current_enviro_data.Rdata') #write out the data