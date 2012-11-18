################################################################################
#Scripts to calculate hydrology metrics for current flow data
# C. James.....................7 November 2012

library(SDMTools) #load the necessary library
out.dir = "/home/jc246980/Hydrology.trials/Hydrology metrics/"

#load necessary data

load('/home/jc246980/Hydrology.trials/Aggregate_reach/Output_futures/Qrun_aggregated2reach_1960to1990/Current_dynamic.Rdata')

################################################################################
#Calculate the annual flow and the coefficient of variation in the monthly and annual flows
tdata=Runoff
co.var<-function(x)(100*sd(x)/mean(x))
outmonthcv= apply(tdata,1,FUN=function(x) {co.var(x)})
outannualsum= rowSums(tdata)
tdata=cbind(tdata, outmonthcv)
tdata=cbind(tdata, outannualsum)

colnames(tdata)[13]=  'monthly_cv'
colnames(tdata)[14] = "annual_total"


################################################################################
#Calculate mean monthly flow for 1, 2 and 3 lowest flow months

month1=1/12
month2 = 2/12
month3 = 3/12
month6 = 6/12

p<-c(month1, month2, month3,month6)

lowflow = function(x) {
	outquant=quantile(x,p,na.rm=TRUE,type=8)
	Lowmonth1_thresh=outquant[1]
	Lowmonth2_thresh=outquant[2]
	Lowmonth3_thresh=outquant[3]
	Lowmonth6_thresh=outquant[4]
	
	Lowmonth1=sum(x[which(x<Lowmonth1_thresh)])
	Lowmonth2=sum(x[which(x<Lowmonth2_thresh)])/2
	Lowmonth3=sum(x[which(x<Lowmonth3_thresh)])/3
	Lowmonth6sum=sum(x[which(x<Lowmonth6_thresh)])
	
	total = sum(x)
	
	Sumlowest6=Lowmonth6sum/total
	
	return(c(Lowmonth1, Lowmonth2, Lowmonth3, Sumlowest6)) #return the values

}

tempdata= t(apply(tdata[,1:12],1,lowflow))
tdata=cbind(tdata, tempdata)                       

colnames(tdata)[15]='lowflow_1_month_mean'
colnames(tdata)[16]='lowflow_2_month_mean'
colnames(tdata)[17]='lowflow_3_month_mean'
colnames(tdata)[18]='driest_6_month%_mean'


################################################################################
#Calculate mean monthly flow for 1, 2 and 3 highest flow months

month11<- 11/12
month10<- 10/12
month9<- 9/12

p<-c(month11, month10, month9)

highflow = function(x) {
	outquant=quantile(x,p,na.rm=TRUE,type=8)
	Highmonth1_thresh=outquant[1]
	Highmonth2_thresh=outquant[2]
	Highmonth3_thresh=outquant[3]
	
	Highmonth1=sum(x[which(x>Highmonth1_thresh)])
	Highmonth2=sum(x[which(x>Highmonth2_thresh)])/2
	Highmonth3=sum(x[which(x>Highmonth3_thresh)])/3

	return(c(Highmonth1, Highmonth2, Highmonth3)) #return the values
}

tempdata= t(apply(tdata[,1:12],1,highflow))	
tdata=cbind(tdata, tempdata)   

colnames(tdata)[19]='highflow_1_month_mean'
colnames(tdata)[20]='highflow_2_month_mean'
colnames(tdata)[21]='highflow_3_month_mean'


################################################################################
#average number of zero flow months

zeroflow=NULL


    zeronum = apply(tdata[,1:12],1,function(x) {return(length(x[which(x==0)]))}) 
    tdata=cbind(tdata, zeronum)
    

colnames(tdata)[22]='zeromonth_no'      

      

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


	tempdata= t(apply(tdata[,1:12],1,myfun))
    tdata=cbind(tdata,tempdata[,1]); tdata=cbind(tdata, tempdata[,2]); tdata=cbind(tdata,tempdata[,3]); tdata=cbind(tdata,tempdata[,4]); tdata=cbind(tdata,tempdata[,5])
	

colnames(tdata)[23]='num.month'
colnames(tdata)[24]='total.severity'
colnames(tdata)[25]='max.clust.length'
colnames(tdata)[26]='clust.severity'
colnames(tdata)[27]='month.max.clust'


################################################################################

out.dir = "/home/jc246980/Hydrology.trials/Hydrology metrics/"

write.csv(tdata,paste(out.dir,"Hydrology_metrics_current_1976to2005.csv",sep=''),row.names=F)	
################################################################################





















