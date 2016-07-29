taxa=c('fish','crayfish','frog','turtles')
for (tax in taxa) { cat('\n',tax,'\n')

	#wd=paste('/rdsi/vol07/cciaf/SDM/models_',tax,'/',sep='') 
	wd=paste('/rdsi/vol07/cciaf/SDM/models_All_frog/',sep='') 
	species.dir=paste('/rdsi/vol07/cciaf/SDM/Realized/',tax,'/Clip4North',sep='')

	file.list=list.files(species.dir)
	species=file.list[grep("cur.real.mat",file.list)]
	species=gsub(".cur.real.mat.Rdata",'',species)
	
	tt=c('Species','Catslope','Flow Accum','Seg slope', 'Bioclim 1','bioclim 4','bioclim 5', 'Bioclim 6','bioclim 12','bioclim 15','bioclim 16', 'Bioclim 17','Clust severity',"d2outlet","max cluster length")
	out=matrix(NA,nr=length(species),nc=15)
	colnames(out)=tt
	i=0
	for (spp in species) { cat('.')
		i=i+1
		out[i,1]=gsub('_',' ',spp)
		tdata=read.csv(paste(wd,spp,'/output/maxentResults.crossvalide.csv', sep=''),as.is=TRUE)
		out[i,2]=tdata$Catslope.permutation.importance[11]
		out[i,3]=tdata$Flow_accum_annual.permutation.importance[11]
		out[i,4]=tdata$Segslope.permutation.importance[11]
		out[i,5]=tdata$bioclim_01.permutation.importance[11]
		out[i,6]=tdata$bioclim_04.permutation.importance[11]
		out[i,7]=tdata$bioclim_05.permutation.importance[11]
		out[i,8]=tdata$bioclim_06.permutation.importance[11]
		out[i,9]=tdata$bioclim_12.permutation.importance[11]
		out[i,10]=tdata$bioclim_15.permutation.importance[11]
		out[i,11]=tdata$bioclim_16.permutation.importance[11]
		out[i,12]=tdata$bioclim_17.permutation.importance[11]
		out[i,13]=tdata$clust.severity.permutation.importance[11]
		out[i,14]=tdata$d2outlet.permutation.importance[11]
		out[i,15]=tdata$max.clust.length.permutation.importance[11]

	}
	write.csv(out,paste('/home/jc246980/SDM/Paper_images/',tax,'_var_imp_table.csv',sep=''), row.names=FALSE)

}

# analysis of differences in drivers between increasers and decreasers on local computer

data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"
tdata=read.csv(paste(data.dir,"SigDiffFishtrans.csv",sep=''))
 gp1=tdata[which(tdata$X=="Inc"),]
 gp2=tdata[which(tdata$X=="Dec"),]
 
 t.test(gp1$Catslope,gp2$Catslope)
 t.test(gp1$Flow.Accum,gp2$Flow.Accum)
 t.test(gp1$Seg.slope,gp2$Seg.slope)
 t.test(gp1$Bioclim.1,gp2$Bioclim.1)
 t.test(gp1$bioclim.4,gp2$bioclim.4)
 t.test(gp1$bioclim.5,gp2$bioclim.5)
  t.test(gp1$Bioclim.6,gp2$Bioclim.6) # Sig diff
    t.test(gp1$bioclim.12,gp2$bioclim.12)
  t.test(gp1$bioclim.15,gp2$bioclim.15) # Sig diff
  t.test(gp1$bioclim.16,gp2$bioclim.16)
  t.test(gp1$Bioclim.17,gp2$Bioclim.17)
  t.test(gp1$Clust.severity,gp2$Clust.severity)
   t.test(gp1$d2outlet,gp2$d2outlet)
  t.test(gp1$max.cluster.length,gp2$max.cluster.length)
  

 wilcox.test(tdata$Catslope~tdata$X)
 wilcox.test(tdata$Flow.Accum~tdata$X)
  wilcox.test(tdata$Seg.slope~tdata$X)
  wilcox.test(tdata$Bioclim.1~tdata$X)
    wilcox.test(tdata$bioclim.4~tdata$X)
	 wilcox.test(tdata$bioclim.5~tdata$X)
	  wilcox.test(tdata$Bioclim.6~tdata$X)
	   wilcox.test(tdata$bioclim.12~tdata$X)
	   wilcox.test(tdata$bioclim.15~tdata$X)
	   wilcox.test(tdata$bioclim.16~tdata$X)
	  wilcox.test(tdata$Bioclim.17~tdata$X)
	  wilcox.test(tdata$Clust.severity~tdata$X)
	  wilcox.test(tdata$d2outlet~tdata$X)
	  wilcox.test(tdata$max.cluster.length~tdata$X)
	  
# plot of significant drivers for all species and then for decreasers and increasers separately
data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"
tdata=read.csv(paste(data.dir,"SigDiffTurtles.csv",sep=''))
 gp1=tdata[which(tdata$X=="Inc"),]
 gp2=tdata[which(tdata$X=="Dec"),]
 
png(paste(data.dir,'Variable_importance_Turtles.png',sep=''), width=12,height=16, units="cm", res=300)
par(mar=c(0,3,1,0),mfrow=c(3,1),cex=0.8,oma=c(8,2,1,0.5)) #define the plot parameters
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,100),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,90,"All species")
grp1data=gp1[,2:15]
grp2data=gp2[,2:15]
boxplot(grp1data,axes=FALSE,boxwex = 0.50,frame.plot=TRUE,ylim=c(0,100),xaxt='n')
text(1.5,90,"Increasers")
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
mtext(text="Average variable contribution %",2, line=3,las=3, cex=0.8) 
boxplot(grp2data,boxwex = 0.50,frame.plot=TRUE,las=2,ylim=c(0,100), names=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),cex.axis=0.8)
text(1.5,90,"Decreasers")
dev.off()


# plot of model drivers for all taxa
 
data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"
tdata=read.csv(paste(data.dir,"SigDiffFish.csv",sep=''))

png(paste(data.dir,'Variable_importance_All_Taxa.png',sep=''), width=12,height=20, units="cm", res=300)
par(mar=c(0,2,1,0),mfrow=c(4,1),cex=0.8,oma=c(8,3,1,0.5)) #define the plot parameters

tdata=read.csv(paste(data.dir,"SigDiffFish.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,100),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,90,"Fish")
tdata=read.csv(paste(data.dir,"SigDiffCrays.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,100),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,90,"Crayfish")
tdata=read.csv(paste(data.dir,"SigDiffTurtles.csv",sep=''))
alldata=tdata[,2:15]

boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,100),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,90,"Turtles")
tdata=read.csv(paste(data.dir,"SigDiffFrogs.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,boxwex = 0.50,frame.plot=TRUE,las=2,ylim=c(0,100), names=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),cex.axis=0.8)
text(1.5,90,"Frogs")
mtext(text="Average variable contribution %",side=2,line=1,cex=0.8, outer=TRUE) 

 devoff()  
 
 
# PCoA of predictors for current
library(caret)
data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"
tdata=load(paste(data.dir,"current_enviro_data.Rdata", sep="")) #data is called current
current = current[,c(paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep=''),'max.clust.length','clust.severity','Flow_accum_annual', 'Segslope', 'Catslope', 'd2outlet')] #keep only variables of interest
curr.pca <- prcomp(current,
                 center = TRUE,
                 scale. = TRUE) 
				 
PC1=curr.pca$rotation[,1]
PC2=curr.pca$rotation[,2] 

png(paste(data.dir,'PCoA_Current_Predictor_relationships.png',sep=''), width=21,height=29.7, units="cm", res=300)
par(mar=c(1,2,1,0),mfrow=c(5,2),cex=0.8,oma=c(8,3,1,0.5)) #define the plot parameters

barplot(curr.pca$rotation[,1],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,2],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,3],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,4],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,5],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,6],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,7],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,8],axisnames=FALSE, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,9],names.arg=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),las=2, ylim=c(-0.6, 0.6)) 
barplot(curr.pca$rotation[,10], names.arg=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),las=2, ylim=c(-0.6, 0.6)) 
 
 dev.off()

 
 ############################
 # extracting jackknife results from maxtent results file
 
 taxa=c('fish','crayfish','frog','turtles')
for (tax in taxa) { cat('\n',tax,'\n')

	#wd=paste('/rdsi/vol07/cciaf/SDM/models_',tax,'/',sep='') 
	wd=paste('/rdsi/vol07/cciaf/SDM/models_turtles/',sep='') 
	species.dir=paste('/rdsi/vol07/cciaf/SDM/Realized/',tax,'/Clip4North',sep='')

	file.list=list.files(species.dir)
	species=file.list[grep("cur.real.mat",file.list)]
	species=gsub(".cur.real.mat.Rdata",'',species)
	
	tt=c('Species','Catslope','Flow Accum','Seg slope', 'Bioclim 1','bioclim 4','bioclim 5', 'Bioclim 6','bioclim 12','bioclim 15','bioclim 16', 'Bioclim 17','Clust severity',"d2outlet","max cluster length")
	out=matrix(NA,nr=length(species),nc=15)
	colnames(out)=tt
	i=0
	out=as.data.frame(out)
	for (spp in species) { cat('.')
		i=i+1
		out[i,1]=gsub('_',' ',spp)
		tdata=read.csv(paste(wd,spp,'/output/maxentResults.csv', sep=''),as.is=TRUE)
		ddata=tdata[,c(grep("Training.gain.with.only",colnames(tdata)))]
		out[i,2:15]<-ddata[1,]

	}
	write.csv(out,paste('/home/jc246980/SDM/Paper_images/',tax,'_Training.gain.with.only.var.csv',sep=''), row.names=FALSE)

}

 
  # plotting maxtent results
 
 # plot of model drivers for all taxa - jackknife results of single variable contribution
 
data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"


png(paste(data.dir,'Training_gain_with_only_All_Taxa.png',sep=''), width=12,height=20, units="cm", res=300)
par(mar=c(0,2,1,0),mfrow=c(4,1),cex=0.8,oma=c(8,3,1,0.5)) #define the plot parameters

tdata=read.csv(paste(data.dir,"fish_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,6),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,5.5,"Fish")

tdata=read.csv(paste(data.dir,"crayfish_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,6),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,5.5,"Crayfish")

tdata=read.csv(paste(data.dir,"turtles_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,6),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,5.5,"Turtles")

tdata=read.csv(paste(data.dir,"frog_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,boxwex = 0.50,frame.plot=TRUE,las=2,ylim=c(0,6), names=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),cex.axis=0.8)
text(1.5,5.5,"Frogs")
mtext(text="Training gain when variable is used in isolation",side=2,line=1,cex=0.8, outer=TRUE) 

 dev.off()  
 
 # plot of model drivers for all taxa - jackknife results of decrease in gains when variable is removed

png(paste(data.dir,'Decrease in training gain.png',sep=''), width=12,height=20, units="cm", res=300)
par(mar=c(0,2,1,0),mfrow=c(4,1),cex=0.8,oma=c(8,3,1,0.5)) #define the plot parameters

tdata=read.csv(paste(data.dir,"fish_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,10),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,9,"Fish")

tdata=read.csv(paste(data.dir,"crayfish_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,10),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,9,"Crayfish")

tdata=read.csv(paste(data.dir,"turtles_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,axes=FALSE, boxwex = 0.50,frame.plot=TRUE,ylim=c(0,10),xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,labels=FALSE,xaxt='n')
text(1.5,9,"Turtles")

tdata=read.csv(paste(data.dir,"frog_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
boxplot(alldata,boxwex = 0.50,frame.plot=TRUE,las=2,ylim=c(0,10), names=c("Catslope","Flow accum", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length"),cex.axis=0.8)
text(1.5,9,"Frogs")
mtext(text="Decrease in training gain when variable is omitted",side=2,line=1,cex=0.8, outer=TRUE) 

 dev.off()  
 
 
 
  #plot of model drivers for all taxa - jackknife results of single variable contribution
 
data.dir="C:/Users/jc246980/Documents/Documents (2)/Freshwater refugia project/Paper drafts/Species shifts/"


png(paste(data.dir,'Training_gain_with_only_All_Taxa.png',sep=''), width=20,height=22, units="cm", res=300)
par(mar=c(3,2,1,0),mfrow=c(2,2),cex=0.8,oma=c(2,8,1,1)) #define the plot parameters

labels= c("Catslope","Flow accumulation", "Segslope", "Annual mean temp","Temp seasonality","Max temp","Min temp","Annual precip",
"Precip seasonality","Precip of wet quarter", "Precip of dry quarter","Clust severity","d2outlet","Max cluster length")



tdata=read.csv(paste(data.dir,"fish_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="Fish",xlab="Regularized training gain", horiz=TRUE, col="lightBlue", las=1, cex.axis=0.8, names.arg=labels, cex.names=1.1) 
tdata=read.csv(paste(data.dir,"fish_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="",xlab="", ylab="",col="Blue",horiz=TRUE, add=TRUE, xaxt="n",yaxt="n") 

tdata=read.csv(paste(data.dir,"crayfish_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="Crayfish",xlab="Regularized training gain", horiz=TRUE, col="lightBlue", yaxt="n",cex.axis=0.8) 
tdata=read.csv(paste(data.dir,"crayfish_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="",xlab="", ylab="",col="Blue",horiz=TRUE, add=TRUE, xaxt="n",yaxt="n") 

tdata=read.csv(paste(data.dir,"turtles_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="Turtles",xlab="Regularized training gain", horiz=TRUE, col="lightBlue", las=1,cex.axis=0.8, names.arg=labels, cex.names=1.1) 
tdata=read.csv(paste(data.dir,"turtles_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="",xlab="", ylab="",col="Blue",horiz=TRUE, add=TRUE, xaxt="n",yaxt="n") 

tdata=read.csv(paste(data.dir,"frog_Training.gain.without.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="Frogs",xlab="Regularized training gain", horiz=TRUE, col="lightBlue", yaxt="n",cex.axis=0.8) 
tdata=read.csv(paste(data.dir,"frog_Training.gain.with.only.var.csv",sep=''))
alldata=tdata[,2:15]
tada=colMeans(alldata)
barplot(tada, main="",xlab="", ylab="",col="Blue",horiz=TRUE, add=TRUE, xaxt="n",yaxt="n") 

mtext("Regularized training gain", side = 1, line = 0, outer=TRUE, cex=1)

 dev.off()  
 
 # plot of model drivers for all taxa - jackknife results of decrease in gains when variable is removed
