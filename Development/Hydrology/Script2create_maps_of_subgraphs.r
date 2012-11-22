###################################################################################################
###Code to plot graphs
###Set up base files
out.dir="/home/jc246980/Hydrology.trials/Flow_accumulation/"
wd = '/home/jc165798/working/NARP_hydro/stability/OZ_5km/data/'; setwd(wd) #define and set working directory
base.asc = read.asc.gz('base.asc.gz')
baseasc=base.asc

gt=gg[[2]]

	png(paste(out.dir,'trial_graph_plot.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
	plot(gt, layout=layout.fruchterman.reingold, vertex.size=4,
	vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
	dev.off()

	lay <- layout.reingold.tilford(gt, root="0")

	
	png(paste(out.dir,'trial_graph_plot2.png',sep=''),width=dim(baseasc)[1]*2+30, height=dim(baseasc)[1]*2+80, units='px', pointsize=20, bg='white')
	plot(gt, layout=lay, vertex.size=5, asp=FALSE, vertex.color=NA,vertex.frame.color=NA)
	dev.off()



	
	
	

GRAPHs = 1:10994

complex=NULL

	for (g in GRAPHs){
		gt=gg[[g]]
		c=length(get.diameter(gt))
		complex=rbind(complex,c)
	}
uid=as.data.frame(1:10994)	
complex=cbind(uid, complex)