###### script to look at where species are moving FROM and TO, and a pic of all the species results together.
#rm(list=ls()) 
################################################################################
	library(SDMTools)#load the necessary libraries
	taxa = c("amphibians", "birds","mammals","reptiles") #;	tax = taxa[1]
	sdm.dir = '/home/jc214262/Refugia/SDM_1km/'
	base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.asc.gz') #read in the base asc file
	pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
	out.dir = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/'
	pic.dir = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/pics/'
	base.asc[which(is.finite(base.asc))] = 0
	#for (tax in taxa){
	tax = taxa[1]
	invaders = contractors = base.asc			
	work.dir=paste(sdm.dir,tax,'/models/',sep="") ; setwd(work.dir)
	species = list.files(work.dir) #get a list of species

#make a table of each species and its range size (# grid cells its present)
	for (spp in species) { print(spp)
	spp.dir = paste(work.dir,spp,"/",sep=''); setwd(spp.dir)
#	if(file.exists('output/maxentResults.csv')){
	thresdir = paste('/home/jc214262/Refugia/SDM_1k_old_stuffed_climate_data/',tax,'/models/',spp,'/',sep='')
	threshold = read.csv(paste(thresdir,'output/maxentResults.csv',sep=''))#; threshold = threshold[which(threshold$Species==spp),]
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value
		
	summary_file = paste(work.dir,spp,'/output/ascii/',sep='')
	asc1990 = paste(summary_file,'1990.asc.gz',sep='')  
	#if(file.exists(asc1990)) {  
	asc1990 = read.asc.gz(asc1990)
	realasc2085 = paste(spp,"_2085_median.asc.gz",sep='')
	#make binary
	asc1990[which(asc1990<threshold)] = 0; asc1990[which(asc1990>0)] = 1
	realasc2085[which(realasc2085<threshold)] = 0; realasc2085[which(realasc2085>0)] = 1
	#add the binary 2015 and 2085 ascis together
	
	change_areas = realasc2085 - asc1990
	new_areas = lost_areas = change_areas
	
	new_areas[which(is.finite(new_areas) & new_areas<0)] = 0
	lost_areas[which(is.finite(lost_areas) & lost_areas>0)] = 0
	lost_areas[which(is.finite(lost_areas) & lost_areas<0)] = 1
		   
	invaders = invaders + new_areas ; range(invaders,na.rm=T)
	contractors = contractors + lost_areas ; range(contractors,na.rm=T)
	}
	
	write.asc.gz(invaders,paste(out.dir,tax,"_invaders.asc",sep=''))
	write.asc.gz(contractors,paste(out.dir,tax,"_contractors.asc",sep=''))
	
	
#################################  all species invaders and contractors

library(SDMTools)#load the necessary libraries
taxa = c("amphibians", "birds","mammals","reptiles") ;	tax = taxa[1]
wd = '/home/jc214262/Refugia/species_richness/1km/invaders_contractors/';setwd(wd)
outdir = '4_taxa_combined/'
## invaders:
ai = read.asc.gz('amphibians_invaders.asc.gz')
bi = read.asc.gz('birds_invaders.asc.gz')
mi = read.asc.gz('mammals_invaders.asc.gz')
ri = read.asc.gz('reptiles_invaders.asc.gz')
	
all_inv = ai + bi + mi + ri
write.asc.gz(all_inv,"all_taxa_invaders.asc")
## contractors:
ac = read.asc.gz('amphibians_contractors.asc.gz')
bc = read.asc.gz('birds_contractors.asc.gz')
mc = read.asc.gz('mammals_contractors.asc.gz')
rc = read.asc.gz('reptiles_contractors.asc.gz')
	
all_cont = ac + bc + mc + rc
write.asc.gz(all_cont,paste(outdir,"all_taxa_contractors.asc",sep=''))
		
	
	
	