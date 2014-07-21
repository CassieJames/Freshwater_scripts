#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments
#sample data
#spp="Acanthopagrus_australis"

################################################################################
# do the work
wd = paste(wd,spp,'/',sep=''); setwd(wd) #define and set working directory
outdir = paste(wd,'summary/',sep=''); dir.create(outdir) #create the output dir
ESs = c('RCP3PD','RCP45','RCP6','RCP85') #define the RCPs
projs = list.files(paste(wd,'output/potential/',sep=''))

for (es in ESs) { cat(es,'\n') #cyle through each of the es
tproj = projs[grep(es,projs)] #get a list of projections for this es
out = as.numeric(system(paste('cut -d, -f1 output/potential/',tproj[1],sep=''),intern=TRUE)[-1]) #get the SegmentNo
out = matrix(out,nrow=length(out),ncol=length(tproj)+2) #setup the output matrix
tt = c('SegmentNo','current_1990',gsub('.csv','',tproj)); tt = gsub(paste(es,'_',sep=''),'',tt); colnames(out) = tt #define the column names
out[,'current_1990'] = as.numeric(system('cut -d, -f3 output/potential/current_1990.csv',intern=TRUE)[-1]) #append the current
for (coi in colnames(out)[-c(1:2)]) { cat('\t',coi,'\n'); out[,coi] = as.numeric(system(paste('cut -d, -f3 output/potential/',es,'_',coi,'.csv',sep=''),intern=TRUE)[-1]) } #append all the future
pot.mat = out; rm(out); gc() #rename the data
save(pot.mat,file=paste(outdir,es,'.pot.mat.Rdata',sep='')); rm(pot.mat); gc() #write out the data
}

