### Files for submission to JCU public data depository

wd = '/home/jc246980/SDM/Environmental_future/'; setwd(wd)    # define and set working directory
out.dir="/home/jc246980/SDM/Environmental_futures_final/"

files=list.files(wd)
files=files

for (f in files) {cat(f,'\n') 	

data=read.csv(f)
colnames(data)[grep("num.month",colnames(data))]<- c("total.length")
data=data[,-c(grep("Segslope",colnames(data)))]
data=data[,-c(grep("Catslope",colnames(data)))]
data=data[,-c(grep("d2outlet",colnames(data)))]
data=data[,-c(grep("lat",colnames(data)))]
data=data[,-c(grep("lon",colnames(data)))]
cois=c('Flow_accum_annual','total.severity', 'clust.severity')
data[,cois] = round(data[,cois],0)
cois=c('bioclim_02')
data[,cois] = round(data[,cois],1)
cois=c('bioclim_03', 'bioclim_15')
data[,cois] = round(data[,cois],3)

write.csv(data,paste(out.dir,f,sep=''),row.names=F)	
}

