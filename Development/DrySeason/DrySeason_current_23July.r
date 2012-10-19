################################################################################
#Script for determining dry season severity
################################################################################

library(SDMTools) #load the necessary library
data.dir = "/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/";  setwd(data.dir)
dataraw.dir = "/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/"

out.dir = "/home/jc246980/DrySeason/Currentdat"
# load current monthly means and sd


################################################################################
#Current dry season severity (based on the 0.8 percentile of the annual summed rainfall)
#calculations based on percentiles determined for each year in 1976-2005

predata = read.csv(paste(dataraw.dir,"rain19402009.csv",sep=''),as.is=TRUE)
precur = as.matrix(predata)                                                     #read in the current monthly data
cois = c(1,2); for (yy in 1976:2005) {cois = c(cois,grep(yy,colnames(precur)))} #get the columns in the years of interest
precur = precur[,cois]                                                          #subset the current data set to the yoi


myfun = function(x) {
tsum = sum(x,na.rm=TRUE) #sum rainfall across all months
threshold = tsum / 12 #threshold defined as rainfall if rainfall was equal across all months of the year
num.month.sub.threshold = length(which(x<threshold)) #get the number of months above a threshold
sum.severity = sum(threshold-x[which(x<threshold)]) #sum of severity for those months
clusters = extreme.clust(c(x,x),threshold,"<=") #get the clusters ... consecutive months below the threshold allowing for clusters to be dec/jan
tt = aggregate(clusters,list(clusters),length) #aggregate by clusters
tt$severity = NA #set the severity to NA
for (clust in tt$Group.1) {
tt$severity[clust] = sum(threshold - c(x,x)[which(clusters==clust)]) #get the wseverities for each cluster
}
clust.length = max(tt$x) #get the max cluster length
clust.severity = max(tt$severity) #get the max cluster severity
month.max.clust = which(clusters%in%tt$Group.1[which(tt$severity==clust.severity)])[1] #give the first month of the most severe cluster
return(c(num.month=num.month.sub.threshold,total.severity=sum.severity,max.clust.length=clust.length,clust.severity=clust.severity,month.max.clust=month.max.clust )) #return the values

}


extreme.clust = function(x,y,thresh.type){
	if (thresh.type=='>=') z = which(x>=y)
	if (thresh.type=='>') z = which(x>y)
	if (thresh.type=='<=') z = which(x<=y)
	if (thresh.type=='<') z = which(x<y)
	if (thresh.type=='==') z = which(x==y)
	if (length(z)>1){
		z.diff = diff(z)
		z.clust = NULL; tclust=1
		for (i in 1:length(z.diff)){
			z.clust = c(z.clust,tclust)
			if (z.diff[i]>1) tclust = tclust + 1
			if (i == length(z.diff)) z.clust = c(z.clust,tclust)
		}
		out = rep(NA,length(x))
		out[z] = z.clust
		return(out)
	} else if (length(z)==1) {
		out = rep(NA,length(x))
		out[z] = 1
		return(out)
	} else {
		return(NULL)
	}
}

precursub=precur
num.month= matrix(NA,nrow=nrow(precursub),ncol=30)                               #create matrix to store values

colnames(num.month) = 1976:2005                                                 #add the column names
month.max.clust=total.severity=max.clust.length=clust.severity = num.month
yois=1976:2005 #yois=1976:2005


for (yy in 1:length(yois)) {  cat(yois[yy],'\n')
      tdata= t(apply(precursub[,grep(yois[yy],colnames(precursub))],1,myfun))
      num.month[,yy]=tdata[,1]; total.severity[,yy]=tdata[,2]; max.clust.length[,yy]=tdata[,3]; clust.severity[,yy]=tdata[,4]; month.max.clust[,yy]=tdata[,5]
	    }


num.month.mean = rowMeans(num.month, na.rm = TRUE); num.month=cbind(num.month, num.month.mean)
num.month.sd = apply(num.month,1,sd); num.month=cbind(num.month, num.month.sd)

total.severity.mean = rowMeans(total.severity, na.rm = TRUE); total.severity=cbind(total.severity, total.severity.mean)
total.severity.sd = apply(total.severity,1,sd); total.severity=cbind(total.severity, total.severity.sd)

max.clust.length.mean = rowMeans(max.clust.length, na.rm = TRUE); max.clust.length=cbind(max.clust.length, max.clust.length.mean)
max.clust.length.sd = apply(max.clust.length,1,sd); max.clust.length=cbind(max.clust.length, max.clust.length.sd)

clust.severity.mean = rowMeans(clust.severity, na.rm = TRUE); clust.severity=cbind(clust.severity, clust.severity.mean)
clust.severity.sd = apply(clust.severity,1,sd); clust.severity=cbind(clust.severity, clust.severity.sd)

month.max.clust.mean = rowMeans(month.max.clust, na.rm = TRUE); month.max.clust=cbind(month.max.clust, month.max.clust.mean)
month.max.clust.sd = apply(month.max.clust,1,sd); month.max.clust=cbind(month.max.clust, month.max.clust.sd)

save(num.month,file=paste(out.dir,'/DrySeason_num.month.Rdata',sep=''))
save(total.severity,file=paste(out.dir,'/DrySeason_total.severity.Rdata',sep=''))
save(max.clust.length,file=paste(out.dir,'/DrySeason_max.clust.length.Rdata',sep=''))
save(clust.severity,file=paste(out.dir,'/DrySeason_clust.severity.Rdata',sep=''))
save(month.max.clust,file=paste(out.dir,'/DrySeason_month.max.clust.Rdata',sep=''))