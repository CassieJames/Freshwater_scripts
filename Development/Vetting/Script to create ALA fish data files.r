
# load R

library(SDMTools); library(maptools)

ala.data = "/home/jc214262/Refugia/Vert_data/ALA_Vertebrate_data"   
out.dir="/home/jc246980/Species_data/ALA_downloads/Data/Fish/"

FullList = as.data.frame(read.csv(paste(work.dir,"/","Full_Freshwater_Fish_List.csv",sep=""), header = T,sep = ",", fill = T))   # load full species list
ALAfish <- as.matrix(FullList$full_name)

	for (sp in 1:length(ALAfish)) { cat(ALAfish[sp],'\n')
		  speciesname=gsub(' ','_',ALAfish[sp])
		  
		    if(file.exists(paste(ala.data, '/',speciesname,'.csv',sep=''))){
             species.data.ala = read.csv(paste(ala.data, '/',otherfishcsv[sp],sep=''))
			 tdata=read.csv(paste(ala.data, '/',speciesname,'.csv',sep=''))
		     write.csv(tdata,paste(out.dir,speciesname,'.csv',sep=''),row.names=F)       
			}
	}