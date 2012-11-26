###################################################################################################
###Code to plot graphs
###Set up base files
library(SDMTools)
out.dir="/home/jc246980/Hydrology.trials/Flow_accumulation/"
wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz')
baseasc=base.asc

gt=gg[[1061]]

	png(paste(out.dir,'trial_graph_plot2.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
	plot(gt, layout=layout.fruchterman.reingold, vertex.size=4, edge.color="black",
	vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=1)
	dev.off()

E(gt)$HydroID
	
	

GRAPHs = 1:10994

complex=NULL

	for (g in GRAPHs){
		gt=gg[[g]]
		c=length(get.diameter(gt))
		complex=rbind(complex,c)
	}
uid=as.data.frame(1:10867)	
complex=cbind(uid, complex)
#1845429 , vertex.label= as.vector(E(gt)$HydroID)

gt=gg[[1061]]

	png(paste(out.dir,'trial_graph_plot3.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
	plot(gg[[1061]],layout=layout.lgl(gg[[1061]],root=0),vertex.size=1,edge.color="black",edge.arrow.size=0.4)
	dev.off()
	
	png(paste(out.dir,'trial_graph_plot4.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
	plot(gg[[1061]],layout=layout.reingold.tilford(gg[[1061]]),vertex.size=1,edge.color="black",edge.arrow.size=0.4)
	dev.off()