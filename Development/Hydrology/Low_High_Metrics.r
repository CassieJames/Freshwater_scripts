################################################################################
#Scripts to calculate hydrology metrics for current

library(SDMTools) #load the necessary library
data.dir = "/home/jc246980/Hydrology.trials";  setwd(data.dir)
out.dir = "/home/jc246980/Hydrology.trials/Hydrology metrics/"

#load necessary data

load(paste(data.dir,'/Output_1976_2005/Q_run_30yearagg.Rdata',sep=''))

################################################################################
#Calculate monthly means, standard deviations and coeff for current

yois = 1976:2005#define the years of interest
tt = expand.grid(sprintf('%02i',1:12),yois=yois);tt = paste(tt[,1],tt[,2],sep='_'); colnames(Q_run) = tt #add the column names

Q_run_curmetrics_mean=NULL
Q_run_curmetrics_sd=NULL

    for (mm in 1:12) { cat(mm,'\n') #cycle through each of the months

           tdata_curmean = rowMeans(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],na.rm=TRUE) #calculate row mean
           tdata_cursd = apply(Q_run[,which(as.numeric(substr(colnames(Q_run),1,2))==mm)],1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
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
hydrocur = as.matrix(Q_run)
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

hydrocur = as.matrix(Q_run)
cois = c(1,2)
yy=1976:2005

Lowmonth1yoi=Lowmonth2yoi=Lowmonth3yoi=tdatalow6yoi=NULL

Q_run_curmetrics_mean=NULL
Q_run_curmetrics_sd=NULL

month1<- 1/12
month2<- 2/12
month3<- 3/12
month4<- 4/12
month5<- 5/12
month6<- 6/12


p<-c(month1, month2, month3, month4, month5, month6)

for (yy in 1976:2005) {  cat(yy,'\n')
      cois=NULL
      cois = c(cois,grep(yy,colnames(hydrocur)))                                  #get the columns in the years of interest
      tdata = hydrocur[,cois]
      outquant = t(apply(tdata,1,function(x) { return(quantile(x,p,na.rm=TRUE,type=7)) })) #get the percentiles
      
      Lowmonth1=as.matrix(outquant[,1])                                          
      Lowmonth2=as.matrix(outquant[,2])
      Lowmonth3=as.matrix(outquant[,3])
      Lowmonth4=as.matrix(outquant[,4])
      Lowmonth5=as.matrix(outquant[,5])
      Lowmonth6=as.matrix(outquant[,6])
      Sumlowest6=(Lowmonth1+Lowmonth2+Lowmonth3+Lowmonth4+Lowmonth5+Lowmonth6)/sum(tdata)      #calculate lowest 6 months as proportion of total
      
      
      Lowmonth1yoi=cbind(Lowmonth1yoi, Lowmonth1)                       
      Lowmonth2yoi=cbind(Lowmonth2yoi, Lowmonth2)             
      Lowmonth3yoi=cbind(Lowmonth3yoi, Lowmonth3)             
      tdatalow6yoi=cbind(tdatalow6yoi,Sumlowest6)
}      
      
      
      Lowest2 = cbind(Lowmonth1yoi,Lowmonth2yoi)
      Lowest3 = cbind(Lowest2,Lowmonth3yoi)
      
      tdata_curmean = rowMeans(Lowmonth1yoi) #calculate row means
      tdata_cursd = apply(Lowmonth1yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)
      
      tdata_curmean = rowMeans(Lowest2) #calculate row means
      tdata_cursd = apply(Lowest2,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)
      
      tdata_curmean = rowMeans(Lowest3) #calculate row means
      tdata_cursd = apply(Lowest3,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)
      
      tdata_curmeans=rowMeans(tdatalow6yoi) 
      tdata_cursd = apply(tdatalow6yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)})
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)
      

colnames(Q_run_curmetrics_mean[,16])='lowflow_1_month_mean'
colnames(Q_run_curmetrics_mean[,17])='lowflow_2_month_mean'
colnames(Q_run_curmetrics_mean[,18])='lowflow_3_month_mean'
colnames(Q_run_curmetrics_mean[,19])='Driest_6_month%_mean'

colnames(Q_run_curmetrics_sd[,15])='lowflow_1_month_sd'
colnames(Q_run_curmetrics_sd[,16])='lowflow_2_month_sd'
colnames(Q_run_curmetrics_sd[,17])='lowflow_3_month_sd'
colnames(Q_run_curmetrics_sd[,18])='Driest_6_month%_sd'



################################################################################
#Calculate mean monthly flow for 1, 2 and 3 highest flow months

hydrocur = as.matrix(Q_run)
cois = c(1,2)
yy=1976:2005

month1<- 1/12
month2<- 2/12
month3<- 3/12
p<-c(month1, month2, month3)

Highmonth1yoi=Highmonth2yoi=Highmonth3yoi=NULL

for (yy in 1976:2005) {  cat(yy,'\n')
      cois = c(cois,grep(yy,colnames(hydrocur)))                                  #get the columns in the years of interest
      tdata = hydrocur[,cois]
      outquant = t(apply(tdata,1,function(x) { return(quantile(x,p,na.rm=TRUE,type=7)) })) #get the percentiles
      Highmonth1=as.matrix(outquant[,1])
      Highmonth2=as.matrix((outquant[,2])
      Highmonth3=as.matrix((outquant[,3])
      
      Highmonth1yoi=cbind(Highmonth1yoi, Highmonth1)
      Highmonth2yoi=cbind(Highmonth2yoi, Highmonth2)
      Highmonth3yoi=cbind(Highmonth3yoi, Highmonth3)
}
      
      Highest2 = cbind(Highmonth1yoi,Highmonth2yoi)
      Highest3 = cbind(Highest2,Highmonth3yoi)
      
      
      tdata_curmean = rowMeans(Highmonth1yoi) #calculate row means
      tdata_cursd = apply(Highmonth1yoi,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

      tdata_curmean = rowMeans(Highest2) #calculate row means
      tdata_cursd = apply(Highest2,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)

      tdata_curmean = rowMeans(Highest3) #calculate row means
      tdata_cursd = apply(Highest3,1,FUN=function(x) {sd(x,na.rm=TRUE)}) #calculate row standard deviation
      Q_run_curmetrics_mean=cbind(Q_run_curmetrics_mean,tdata_curmean)
      Q_run_curmetrics_sd=cbind(Q_run_curmetrics_sd,tdata_cursd)


colnames(Q_run_curmetrics_mean[,20])='highflow_1_month_mean'
colnames(Q_run_curmetrics_mean[,21])='highflow_2_month_mean'
colnames(Q_run_curmetrics_mean[,22])='highflow_3_month_mean'

colnames(Q_run_curmetrics_sd[,19])='highflow_1_month_sd'
colnames(Q_run_curmetrics_sd[,20])='highflow_2_month_sd'
colnames(Q_run_curmetrics_sd[,21])='highflow_3_month_sd'


################################################################################
#Number of zero flow months

#zero<-function(x)(100*sd(x)/mean(x))
zeroflow=NULL
hydrocur = as.matrix(Q_run)

yy=1976:2005
    for (yy in 1976:2005) {  cat(yy,'\n')
         
          cois=NULL
          cois = c(cois,grep(yy,colnames(hydrocur)))                                  #get the columns in the years of interest
          tdata = hydrocur[,cois]
          zeronum = apply(tdata,1,function(x) {return(length(x[which(x==0)]))}) 
          zeroflow=cbind(zeroflow, zeronum)
    }


colnames(Q_run_curmetrics_mean[,23])='Zeroflow_mean'      
colnames(Q_run_curmetrics_sd[,22])='Zeroflow_sd'      
      
################################################################################







































