################################################################################
#Scripts to calculate hydrology metrics for current

library(SDMTools) #load the necessary library
data.dir = "/home/jc246980/Hydrology.trials";  setwd(data.dir)
out.dir = "/home/jc246980/Hydrology.trials/Hydrology metrics/"

#load necessary data

load(paste(data.dir,'/Outputs/Output_1976_2005/Q_run_30yearagg_dynamic.Rdata',sep=''))

################################################################################
#Calculate monthly means, standard deviations and coeff for current

yois = 1976:2005#define the years of interest
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Qrun) = tt #add the column names

Q_run_curmetrics_mean=NULL
Q_run_curmetrics_sd=NULL

    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],na.rm=TRUE) #calculate row mean
           tdata_cursd = apply(Qrun[,which(as.numeric(substr(colnames(Qrun),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
           #tdata_covar =  100*(tdata_cursd/tdata_curmean)
           Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
           Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)
           #Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_covar)
    }

tt = expand.grid(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),c('mean'));tt = paste(tt[,1],tt[,2],sep='_'); colnames(Q_run_curmetrics_mean)=  tt
tt = expand.grid(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),c('sd'));tt = paste(tt[,1],tt[,2],sep='_'); colnames(Q_run_curmetrics_sd)=  tt

################################################################################
#Calculate the annual flow and the coefficient of variation in the monthly and annual flows

co.var<-function(x)(100*sd(x)/mean(x))
tempcv=tempsum=NULL
hydrocur = as.matrix(Qrun)
cois = c(1)
yy=1976:2005
    for (yy in 1976:2005) {  cat(yy,'\n')
          cois=NULL
          cois = c(cois,grep(yy,colnames(hydrocur)))                            #get the columns in the years of interest
          tdata = hydrocur[,cois]
          outmonthcv= apply(tdata,1,FUN=function(x) {co.var(x)})
          outannualsum= rowSums(tdata)
          tempcv=cbind(tempcv, outmonthcv)
          tempsum=cbind(tempsum, outannualsum)
    }

tdatameans= as.matrix(rowMeans(tempsum))                                        #mean of the annual total
tdatameansd= as.matrix( apply(tempsum,1,FUN=function(x) {sd(x,na.rm=TRUE)}))    #sd of the annual total
tdatacv= as.matrix(apply(tempsum,1,FUN=function(x) {co.var(x)}))                #CV of the annual total   - there are NAs is here where the mean is ZERO

Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdatameans)
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdatacv)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdatameansd)


colnames(Q_run_curmetrics_mean)[13] = "Annual_mean"
colnames(Q_run_curmetrics_sd)[13]=  'Annual_mean_sd'
colnames(Q_run_curmetrics_mean)[14]=  'Annual_cv'


tdatameans= as.matrix(rowMeans(tempcv))      #mean of the monthly cv
tdatacvsd= as.matrix(apply(tempcv,1,FUN=function(x) {sd(x,na.rm=TRUE)}))     #sd of the monthly cv


Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdatameans)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdatacv)

colnames(Q_run_curmetrics_mean)[15]=  'Monthly_cv_mean'
colnames(Q_run_curmetrics_sd)[14]=  'Monthly_cv_sd'

################################################################################
#Calculate mean monthly flow for 1, 2 and 3 lowest flow months


hydrocur = as.matrix(Qrun)
cois = c(1,2)
yy=1976:2005
Lowmonth1yoi=Lowmonth2yoi=Lowmonth3yoi=tdatalow6yoi=NULL

month1=1/12
month2 = 2/12
month3 = 3/12
month6 = 6/12

p<-c(month1, month2, month3,month6)

	for (yy in 1976:2005) {  cat(yy,'\n')

		  cois=NULL
		  cois = c(cois,grep(yy,colnames(hydrocur)))                                  #get the columns in the years of interest
		  tdata = hydrocur[,cois]
		  outquant = t(apply(tdata,1,function(x) { return(quantile(x,p,na.rm=TRUE,type=7)) })) #get the percentiles
		  
		  Lowmonth1_thresh=outquant[,1]
		  Lowmonth1 = apply(tdata,1,FUN=function(x) {sum(x[which(x<Lowmonth1_thresh[1])])}) 
	  
		  Lowmonth2_thresh=outquant[,2]
		  Lowmonth2 = apply(tdata,1,FUN=function(x) {sum(x[which(x<Lowmonth2_thresh[1])])/2}) #for each row sums all values below threshold and divides by number of months to get mean
		  
		  Lowmonth3_thresh=outquant[,3]
		  Lowmonth3 = apply(tdata,1,FUN=function(x) {sum(x[which(x<Lowmonth3_thresh[1])])/3}) #for each row sums all values below threshold and divides by number of months to get mean
		   
		  Lowmonth6_thresh=outquant[,4]
		  Lowmonth6sum = apply(tdata,1,FUN=function(x) {sum(x[which(x<Lowmonth6_thresh[1])])}) 
		  
		  total = apply(tdata,1,sum)
		  
		  Sumlowest6=Lowmonth6sum/total  #calculate sum of lowest 6 months as proportion of total
			
		  
		  
		  Lowmonth1yoi=cbind(Lowmonth1yoi, Lowmonth1)                       
		  Lowmonth2yoi=cbind(Lowmonth2yoi, Lowmonth2)             
		  Lowmonth3yoi=cbind(Lowmonth3yoi, Lowmonth3)             
		  tdatalow6yoi=cbind(tdatalow6yoi,Sumlowest6)
	}      
      
      
  

tdata_curmean = rowMeans(Lowmonth1yoi) #calculate row means
tdata_cursd = apply(Lowmonth1yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

tdata_curmean = rowMeans(Lowmonth2yoi) #calculate row means
tdata_cursd = apply(Lowmonth2yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

tdata_curmean = rowMeans(Lowmonth3yoi) #calculate row means
tdata_cursd = apply(Lowmonth3yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

tdata_curmean=rowMeans(tdatalow6yoi) 
tdata_cursd = apply(tdatalow6yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)})
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)


colnames(Q_run_curmetrics_mean)[16]='lowflow_1_month_mean'
colnames(Q_run_curmetrics_mean)[17]='lowflow_2_month_mean'
colnames(Q_run_curmetrics_mean)[18]='lowflow_3_month_mean'
colnames(Q_run_curmetrics_mean)[19]='Driest_6_month%_mean'

colnames(Q_run_curmetrics_sd)[15]='lowflow_1_month_sd'
colnames(Q_run_curmetrics_sd)[16]='lowflow_2_month_sd'
colnames(Q_run_curmetrics_sd)[17]='lowflow_3_month_sd'
colnames(Q_run_curmetrics_sd)[18]='Driest_6_month%_sd'



################################################################################
#Calculate mean monthly flow for 1, 2 and 3 highest flow months

hydrocur = as.matrix(Qrun)
cois = c(1,2)
yy=1976:2005

month11<- 11/12
month10<- 10/12
month9<- 9/12
p<-c(month11, month10, month9)

Highmonth1yoi=Highmonth2yoi=Highmonth3yoi=NULL

	for (yy in 1976:2005) {  cat(yy,'\n')
		  yy=1976
		  cois=NULL
		  cois = c(cois,grep(yy,colnames(hydrocur))) #get the columns in the years of interest
		  tdata = hydrocur[,cois]
		  outquant = t(apply(tdata,1,function(x) { return(quantile(x,p,na.rm=TRUE,type=8)) })) #get the quantiles
		  
		  Highmonth1_thresh=outquant[,1]
		  Highmonth1 = apply(tdata,1,FUN=function(x) {sum(x[which(x>Highmonth1_thresh[1])])}) 
		  
		  Highmonth2_thresh=outquant[,2]
		  Highmonth2 = apply(tdata,1,FUN=function(x) {sum(x[which(x>Highmonth2_thresh[2])])/2}) 
		  
		  Highmonth3_thresh=outquant[,3]
		  Highmonth3 = apply(tdata,1,FUN=function(x) {sum(x[which(x>Highmonth3_thresh[3])])/3}) 
		  
		  Highmonth1yoi=cbind(Highmonth1yoi, Highmonth1)
		  Highmonth2yoi=cbind(Highmonth2yoi, Highmonth2)
		  Highmonth3yoi=cbind(Highmonth3yoi, Highmonth3)
	}
		  

      
      
      tdata_curmean = rowMeans(Highmonth1yoi) #calculate row means
      tdata_cursd = apply(Highmonth1yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

      tdata_curmean = rowMeans(Highmonth2yoi) #calculate row means
      tdata_cursd = apply(Highmonth2yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

      tdata_curmean = rowMeans(Highmonth3yoi) #calculate row means
      tdata_cursd = apply(Highmonth3yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)


colnames(Q_run_curmetrics_mean[,20])='highflow_1_month_mean'
colnames(Q_run_curmetrics_mean[,21])='highflow_2_month_mean'
colnames(Q_run_curmetrics_mean[,22])='highflow_3_month_mean'

colnames(Q_run_curmetrics_sd[,19])='highflow_1_month_sd'
colnames(Q_run_curmetrics_sd[,20])='highflow_2_month_sd'
colnames(Q_run_curmetrics_sd[,21])='highflow_3_month_sd'


################################################################################
#average number of zero flow months

zeroflow=NULL
hydrocur = as.matrix(Qrun)
yy=1976:2005

    for (yy in 1976:2005) {  cat(yy,'\n')
         
          cois=NULL
          cois = c(cois,grep(yy,colnames(hydrocur))) #get the columns in the years of interest
          tdata = hydrocur[,cois]
          zeronum = apply(tdata,1,function(x) {return(length(x[which(x==0)]))}) 
          zeroflow=cbind(zeroflow, zeronum)
    }

tdata_curmean = rowMeans(zeroflow) #calculate row means
tdata_cursd = apply(zeroflow,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

colnames(Q_run_curmetrics_mean[,23])='Zeroflow_mean'      
colnames(Q_run_curmetrics_sd[,22])='Zeroflow_sd'      
      

################################################################################
#Current low flow severity based on a threshold of average monthly flow for a given year

myfun = function(x) {
	tsum = sum(x,na.rm=TRUE) #sum rainfall across all months
	threshold = tsum / 12 #threshold defined as flow  if flow was equal across all months of the year
	num.month.sub.threshold = length(which(x<threshold)) #get the number of months above a threshold
	sum.severity = sum(threshold-x[which(x<threshold)]) #sum of severity for those months
	clusters = extreme.clust(c(x,x),threshold,"<=") #get the clusters ... consecutive months below the threshold allowing for clusters to be dec/jan
	tt = aggregate(clusters,list(clusters),length) #aggregate by clusters
	tt$severity = NA #set the severity to NA
		for (clust in tt$Group.1) {
			tt$severity[clust] = sum(threshold - c(x,x)[which(clusters==clust)]) #get the severities for each cluster
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


hydrocur = as.matrix(Qrun)
num.month= matrix(NA,nrow=nrow(hydrocur),ncol=30)    
colnames(num.month)=  1976:2005
month.max.clust=total.severity=max.clust.length=clust.severity = num.month

yois=1976:2005 

	for (yy in yois)) {  cat(yois[yy],'\n')
		  yy=yois[1]
		  cois=NULL
		  cois = c(cois,grep(yy,colnames(hydrocur)))
		  tdata= t(apply(hydrocur[,cois],1,myfun))
		  num.month[,grep(yy,colnames(num.month))]=tdata[,1]; total.severity[,grep(yy,colnames(total.severity))]=tdata[,2]; max.clust.length[,grep(yy,colnames(max.clust.length))]=tdata[,3]; clust.severity[,grep(yy,colnames(clust.severity))]=tdata[,4]; month.max.clust[,grep(yy,colnames(month.max.clust))]=tdata[,5]
	}

num.month.mean = rowMeans(num.month, na.rm = TRUE); Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean, num.month.mean)
num.month.sd = apply(num.month,1,sd); num.month=cbind(num.month, num.month.sd)

total.severity.mean = rowMeans(total.severity, na.rm = TRUE); Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean, total.severity.mean)
total.severity.sd = apply(total.severity,1,sd); Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd, total.severity.sd)

max.clust.length.mean = rowMeans(max.clust.length, na.rm = TRUE); Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean, max.clust.length.mean)
max.clust.length.sd = apply(max.clust.length,1,sd); Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd, max.clust.length.sd)

clust.severity.mean = rowMeans(clust.severity, na.rm = TRUE); Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean, clust.severity.mean)
clust.severity.sd = apply(clust.severity,1,sd); Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd, clust.severity.sd)

month.max.clust.mean = rowMeans(month.max.clust, na.rm = TRUE); Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean, month.max.clust.mean)
month.max.clust.sd = apply(month.max.clust,1,sd); Q_run_curmetrics_sdt=cbind(Q_run_curmetrics_sd, month.max.clust.sd)


colnames(Q_run_curmetrics_mean[,24])='num.month.mean'
colnames(Q_run_curmetrics_mean[,25])='total.severity.mean'
colnames(Q_run_curmetrics_mean[,26])='max.clust.length.mean'
colnames(Q_run_curmetrics_mean[,27])='clust.severity.mean'
colnames(Q_run_curmetrics_mean[,28])='month.max.clust.mean'


colnames(Q_run_curmetrics_sd[,23])='num.month.sd'
colnames(Q_run_curmetrics_sd[,24])='total.severity.sd'
colnames(Q_run_curmetrics_sd[,25])='max.clust.length.sd'
colnames(Q_run_curmetrics_sd[,26])='clust.severity.sd'
colnames(Q_run_curmetrics_sd[,27])='month.max.clust.sd'

################################################################################

out.dir = "/home/jc246980/Hydrology.trials/Hydrology metrics/Current_1976_2005/"

save(Q_run_curmetrics_mean,file=paste(out.dir,'Q_run_curmetrics_mean.Rdata',sep=''))
save(Q_run_curmetrics_sd,file=paste(out.dir,'Q_run_curmetrics_sd.Rdata',sep=''))

################################################################################





















