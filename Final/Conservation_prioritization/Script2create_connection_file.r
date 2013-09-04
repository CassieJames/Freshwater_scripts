
library(maptools)

networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')

tdata= networkatts[,c(2,9,13)]
connect=matrix(NA, nrow=0, ncol=2)
colnames(connect)=c("SegmentNo", "t")


	for(i in 1:nrow(tdata)) {
		temp=tdata[i,]
		if (temp[3]==-1) {t=-1 # if next down is -1 then just return -1
		}else{ 
		t=tdata[tdata$HydroID==as.numeric(temp[3]),2] # if next down is a hydroID, need to go to row in tdata and find appropriate row and then read off second value (SegmentNo)
		}
		temp2=cbind(temp[2], t)
		connect=rbind(connect,temp2)
		
	}

out.dir="/home/jc246980/Zonation/"
save(connect,file=paste(out.dir,'Zonation_connectivity_ALL.Rdata',sep=''))     # save out file incase it crashes again