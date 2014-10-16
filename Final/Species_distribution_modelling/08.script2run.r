


args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments

library(SDMTools)

# Create pos file and save out
#source('/home/jc148322/scripts/libraries/cool_functions.r')
#base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
#pos=make.pos(base.asc)
#pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),base.asc) 
#sdm.dir='/home/jc246980/SDM/'
#save(pos,file=paste(sdm.dir,'pos1km.Rdata',sep=''))

# emg also, the 'row' and 'col' columns probably aren't necessary although I doubt it will speed anything up
#	if they are removed

#tpos=pos 
# emg is tpos used? I don't think it was before but I use it in the for loop below
load(paste('/home/jc246980/SDM/Realized/',taxon,'/Clip4North/',es,"/",spp, ".fut.real.mat.Rdata",sep=''))

load(paste(sdm.dir,"pos1km.Rdata",sep=''))

# out.dir = paste("/rdsi/vol07/ccimpacts/NRM/", taxon, "/models/", spp, "/1km/future", sep=""); dir.create(out.dir)
# emg my out.dir

for (ea in 2:ncol(real.mat)) {

	
	tpos=pos 
	# emg make a copy to append species info
	
	sc.name = colnames(real.mat)[ea] 
	# emg get the name of the column to use in output filename

	tpos=merge(tpos,real.mat[,c(1,ea)], by='SegmentNo',all.x=TRUE)
# emg all.x if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y.	
# but x has fewer segments ... so pos with a segment that is not in the species file?
	
	outfilename = paste(out.dir,"/", es, "_", sc.name, '.fut.real.grid.Rdata',sep='')
	# emg RCP45_cccma-cgcm31_2015.fut.real.grid.Rdata
	# emg if you want to put all the files in a single output folder you'll need to add in spp name too
	save(tpos, file=outfilename); rm(tpos);gc() #write out the data		
}
