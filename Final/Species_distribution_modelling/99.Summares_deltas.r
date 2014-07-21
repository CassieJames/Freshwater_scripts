###get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments


##get the delta data in the same format as cassie's delta outputs in preparation for input into her stability summary script

###Load necessary libraries
library(SDMTools); library(maptools) #define the libraries needed

###Set up base files
base.asc = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep='')) #read in the base asc file
pos = as.data.frame(which(is.finite(base.asc),arr.ind=T))
pos$lat = getXYcoords(base.asc)$y[pos$col]
pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon

future.dir="/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/"
out.dir="/home/jc246980/SDM/Richness/summaries/deltas/"; dir.create(out.dir,recursive=T)

ESs=c('RCP3PD', 'RCP45', 'RCP6','RCP85'); es=ESs[4]

###01. make a pos file with Segment No and River basins (major drainages)
catchments = read.asc.gz(paste('/home/jc148322/NARPfreshwater/SDM/SegmentNo_1km.asc.gz',sep=''))
pos$SegmentNo=extract.data(cbind(pos$lon,pos$lat),catchments)

RiverBasin.asc = read.asc("/home/jc246980/Janet_Stein_data/ncb_level1.asc") # load river basin asc
pos$Riverbasin = extract.data(cbind(pos$lon,pos$lat), RiverBasin.asc) # Map river basins onto postition file
RiverBasins = unique(na.omit(pos$Riverbasin)) # create river basin vector

basedir='/home/jc246980/SDM/Richness/'

taxa=c('fish','crayfish','frog','turtles')
summaries=NULL
###02. for each taxa, load the percentile richness for future and current and create proportions
for (tax in taxa) { cat(tax,'\n') #cycle through each basin

        load(paste(basedir,'/',es,"_",tax,'_Richness_quants.Rdata',sep=''))					#load the future data
        load(paste('/home/jc246980/SDM/Richness/',tax,'Richness_current.mat.Rdata',sep=''))	#load the current data
        outdelta=outquant_Richness[,2:ncol(outquant_Richness)]# make a copy
		outdelta=outdelta/Richness_current[,2]
		outdelta[which(is.nan(outdelta))]=NA
		outdelta[which(outdelta>2)]=2
		outdelta=cbind(outquant_Richness[,1],outdelta)
		colnames(outdelta)[1]<-"SegmentNo"

###03. merge the percentile richness with pos for each taxa and save the data out

		outdelta=merge(pos,outdelta,by='SegmentNo',all.x=T)


### Create output table

	table_delta = matrix(NA,nrow=length(RiverBasins)*3,ncol=3); #define the output matrix
	colnames(table_delta) = c(10,50,90)
	tt = expand.grid(c('quant_10', 'quant_50', 'quant_90'),RiverBasins); tt = paste(tt[,2],tt[,1],sep='_'); rownames(table_delta)=tt
	

	for (rb in RiverBasins) { cat(rb,'\n') #cycle through each basin
				
				outdelta_rb = outdelta[which(outdelta$Riverbasin==rb),] #get the data only for the rb of interest
				
				outquant = apply(outdelta_rb[,c(28:30)],2,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })#get the percentiles					
				rowname=paste(rb,"_quant_", c(10,50,90), sep='')
				table_delta[rownames(table_delta)==rowname,]=outquant[,]

		}; cat('\n')
		
	if (tax==c("fish")) summaries=table_delta else summaries=cbind(summaries,table_delta[,2:4])
	
}	
	write.csv(summaries,paste(out.dir,"rb_richness_all.csv",sep=''),row.names=T)	

	