taxa=c('fish','crayfish','frog','turtles')
taxa=c('turtles')
for (tax in taxa) { cat('\n',tax,'\n')

	wd=paste('/rdsi/vol07/cciaf/SDM/models_',tax,'/',sep='') 
	species.dir=('/rdsi/vol07/cciaf/SDM/Realized/turtles/Clip4North')
	file.list=list.files(species.dir)
	species=file.list[grep("cur.real.mat",file.list)]
	species=gsub(".cur.real.mat.Rdata",'',species)
	
	tt=c('Species','Number of observations','AUC (mean)','AUC (SD)', 'Threshold','Omission rate')
	out=matrix(NA,nr=length(species),nc=length(tt))
	colnames(out)=tt
	i=0
	for (spp in species) { cat('.')
	i=i+1
		out[i,1]=gsub('_',' ',spp)
		tdata=read.csv(paste(wd,spp,'/output/maxentResults.crossvalide.csv', sep=''),as.is=TRUE)
		
		out[i,2]=round(tdata$X.Training.samples[11]+tdata$X.Test.samples[11])
		if (is.na(out[i,2])) out[i,2]=round(mean(tdata$X.Training.samples)+mean(tdata$X.Test.samples))
		out[i,3]=tdata$Test.AUC[11]
		if (is.na(out[i,3])) out[i,3]=mean(tdata$Test.AUC)
		out[i,4]=tdata$AUC.Standard.Deviation[11]
		if (is.na(out[i,4])) out[i,4]=mean(tdata$AUC.Standard.Deviation)
		tdata=read.csv(paste(wd,spp,'/output/maxentResults.csv', sep=''),as.is=TRUE)
		out[i,5]=tdata$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]
		out[i,6]=tdata$Equate.entropy.of.thresholded.and.original.distributions.training.omission[1]

	}
	write.csv(out,paste('/home/jc246980/SDM/Paper_images/',tax,'_info_table.csv',sep=''), row.names=FALSE)

}