################################################################################
#Calculate future dry season severity
out.dir = "/home/jc246980/DrySeason/Currentdat"
library(SDMTools) #load the necessary library

#load current data
load(paste(out.dir, '/DrySeason_num.month.Rdata', sep=''))  # num.month
load(paste(out.dir, '/DrySeason_total.severity.Rdata', sep='')) # total.severity
load(paste(out.dir, '/DrySeason_max.clust.length.Rdata', sep='')) # max.clust.length
load(paste(out.dir, '/DrySeason_clust.severity.Rdata', sep=''))    # clust.severity
load(paste(out.dir, '/DrySeason_month.max.clust.Rdata',sep=''))   #  month.max.clust

future.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/pre/'
out.dir ="/home/jc246980/DrySeason/Futuredat/"

files=list.files(future.dir, pattern='RCP')
ESs = unlist(strsplit(files,"_")); ESs = unique(ESs[seq(1,length(ESs),2)])#list the emission scenarios
GCMs = list.files(future.dir,pattern=ESs[1]); GCMs = gsub(paste(ESs[1],'_',sep=''),'',GCMs); GCMs = gsub('.Rdata','',GCMs) #get a list of GCMs
YOIS=seq(2015,2085,10)


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







#chug through each es and gcm
for (es in ESs){
      fut.num.month= matrix(NA,nrow=nrow(num.month),ncol=144)
      tt = expand.grid(GCMs,YOIS); tt = paste(tt[,1],tt[,2],sep='_'); colnames(fut.num.month) = tt #add the column names
      #set up ridiculous number of matrix
      sd.num.month=delta.num.month =fut.total.severity=fut.max.clust.length=fut.clust.severity =fut.month.max.clust= fut.num.month
      delta.month.max.clust=delta.total.severity=delta.max.clust.length=delta.clust.severity = delta.num.month
      sd.month.max.clust=sd.total.severity=sd.max.clust.length=sd.clust.severity = sd.num.month

      for (gcm in GCMs){   cat(gcm,'\n')
                load(paste(future.dir,es,'_',gcm,'.Rdata', sep=''))
                for (yy in YOIS){   cat(yy,'\n')

                      tdata= t(apply(futdata[,grep(yy,colnames(futdata))],1,myfun))
                      fut.num.month[,intersect(grep(yy,colnames(fut.num.month)),grep(gcm,colnames(fut.num.month)))]=tdata[,1]
                      fut.total.severity[,intersect(grep(yy,colnames(fut.total.severity)),grep(gcm,colnames(fut.total.severity)))]=tdata[,2]
                      fut.max.clust.length[,intersect(grep(yy,colnames(fut.max.clust.length)),grep(gcm,colnames(fut.max.clust.length)))]=tdata[,3]
                      fut.clust.severity[,intersect(grep(yy,colnames(fut.clust.severity)),grep(gcm,colnames(fut.clust.severity)))]=tdata[,4]
                      fut.month.max.clust[,intersect(grep(yy,colnames(fut.month.max.clust)),grep(gcm,colnames(fut.month.max.clust)))]=tdata[,5]

                      delta.num.month[,intersect(grep(yy,colnames(delta.num.month)),grep(gcm,colnames(delta.num.month)))]=fut.num.month[,intersect(grep(yy,colnames(fut.num.month)),grep(gcm,colnames(fut.num.month)))]-num.month[,31]
                      sd.num.month[,intersect(grep(yy,colnames(sd.num.month)),grep(gcm,colnames(sd.num.month)))]=(fut.num.month[,intersect(grep(yy,colnames(fut.num.month)),grep(gcm,colnames(fut.num.month)))]-num.month[,31])/num.month[,32]+0.000001

                      delta.total.severity[,intersect(grep(yy,colnames(delta.total.severity)),grep(gcm,colnames(delta.total.severity)))]=fut.total.severity[,intersect(grep(yy,colnames(fut.total.severity)),grep(gcm,colnames(fut.total.severity)))]/total.severity[,31]+0.000001
                      sd.total.severity[,intersect(grep(yy,colnames(sd.total.severity)),grep(gcm,colnames(sd.total.severity)))]=(fut.total.severity[,intersect(grep(yy,colnames(fut.total.severity)),grep(gcm,colnames(fut.total.severity)))]-total.severity[,31])/total.severity[,32]+0.000001

                      delta.max.clust.length[,intersect(grep(yy,colnames(delta.max.clust.length)),grep(gcm,colnames(delta.max.clust.length)))]=fut.max.clust.length[,intersect(grep(yy,colnames(fut.max.clust.length)),grep(gcm,colnames(fut.max.clust.length)))]-max.clust.length[,31]
                      sd.max.clust.length[,intersect(grep(yy,colnames(sd.max.clust.length)),grep(gcm,colnames(sd.max.clust.length)))]=(fut.max.clust.length[,intersect(grep(yy,colnames(fut.max.clust.length)),grep(gcm,colnames(fut.max.clust.length)))]-max.clust.length[,31])/max.clust.length[,32]+0.000001

                      delta.clust.severity[,intersect(grep(yy,colnames(delta.clust.severity)),grep(gcm,colnames(delta.clust.severity)))]=fut.clust.severity[,intersect(grep(yy,colnames(fut.clust.severity)),grep(gcm,colnames(fut.clust.severity)))]/clust.severity[,31]+0.000001
                      sd.clust.severity[,intersect(grep(yy,colnames(sd.clust.severity)),grep(gcm,colnames(sd.clust.severity)))]=(fut.clust.severity[,intersect(grep(yy,colnames(fut.clust.severity)),grep(gcm,colnames(fut.clust.severity)))]-clust.severity[,31])/clust.severity[,32]+0.000001

                      delta.month.max.clust[,intersect(grep(yy,colnames(delta.month.max.clust)),grep(gcm,colnames(delta.month.max.clust)))]=fut.month.max.clust[,intersect(grep(yy,colnames(fut.month.max.clust)),grep(gcm,colnames(fut.month.max.clust)))]-month.max.clust[,31]
                      sd.month.max.clust[,intersect(grep(yy,colnames(sd.month.max.clust)),grep(gcm,colnames(sd.month.max.clust)))]=(fut.clust.severity[,intersect(grep(yy,colnames(fut.month.max.clust)),grep(gcm,colnames(fut.month.max.clust)))]-month.max.clust[,31])/month.max.clust[,32]+0.000001
                      }

                }
                save(fut.num.month,file=paste(out.dir,"Data/",es,"_num_month.Rdata",sep=''))
                save(fut.total.severity,file=paste(out.dir,"Data/",es,"_total_severity.Rdata",sep=''))
                save(fut.max.clust.length,file=paste(out.dir,"Data/",es,"_max_clust_length.Rdata",sep=''))
                save(fut.clust.severity,file=paste(out.dir,"Data/",es,"_fut_clust_severity.Rdata",sep=''))
                save(fut.month.max.clust,file=paste(out.dir,"Data/",es,"_fut_month_max_clust.Rdata",sep=''))

                save(delta.num.month,file=paste(out.dir,"Delta/",es,"_delta_num_month.Rdata",sep=''))
                save(delta.total.severity,file=paste(out.dir,"Delta/",es,"_delta_total_severity.Rdata",sep=''))
                save(delta.max.clust.length,file=paste(out.dir,"Delta/",es,"_delta_max_clust_length.Rdata",sep=''))
                save(delta.clust.severity,file=paste(out.dir,"Delta/",es,"_delta_fut_clust_severity.Rdata",sep=''))
                save(delta.month.max.clust,file=paste(out.dir,"Delta/",es,"_delta_month_max_clust.Rdata",sep=''))
                
                save(sd.num.month,file=paste(out.dir,"SD/",es,"_sd_num_month.Rdata",sep=''))
                save(sd.total.severity,file=paste(out.dir,"SD/",es,"_sd_total_severity.Rdata",sep=''))
                save(sd.max.clust.length,file=paste(out.dir,"SD/",es,"_sd_max_clust_length.Rdata",sep=''))
                save(sd.clust.severity,file=paste(out.dir,"SD/",es,"_sd_fut_clust_severity.Rdata",sep=''))
                save(sd.month.max.clust,file=paste(out.dir,"SD/",es,"_sd_month_max_clust.Rdata",sep=''))

 }
################################################################################