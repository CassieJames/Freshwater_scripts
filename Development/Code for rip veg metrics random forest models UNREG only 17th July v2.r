#---------------------------------------------------------------------------
# Programs to produce regression random forest models for rip veg metrics
#---------------------------------------------------------------------------

library(randomForest)
library(vegan)
library(party)
#this is a comment
data.dir = "C:/Users/jc246980/Documents/NWC project/Riparian structure paper/"; setwd (data.dir)

# Main data files

metrics.hab<-data.frame(read.csv(file="RiparianMetrics_reduced.csv"),row.names=T, header=T)
#metrics<-subset(metrics.hab, select=RICH:D_REGEN_EXOTIC)
#habitat<-subset(metrics.hab, select=DECLAT:BFDur)

#habitat.std<-decostand(habitat, "standardize")

#metrics.rng<-decostand(metrics, "range")

# drop highly regulated sites
metrics.hab.nonreg<-subset(metrics.hab[-c(4,6,10,11,20,21,27,28), ])
METRICs<-as.data.frame(subset(metrics.hab.nonreg, select=RICH:D_REGEN_EXOTIC))
habitat.nonreg<-as.data.frame(subset(metrics.hab.nonreg, select=DECLAT:BFDur))

habitat.nonreg.topo<-as.data.frame(subset(metrics.hab.nonreg, select=DECLAT:BFWIDTH_DEP))
habitat.nonreg.climate<-as.data.frame(subset(metrics.hab.nonreg, select=A_RAIN:CMA_TEMP))
habitat.nonreg.substrate<-as.data.frame(subset(metrics.hab.nonreg, select=FELSIC:CLAYS))
habitat.nonreg.landuse<-as.data.frame(subset(metrics.hab.nonreg, select=PNE:PIA))
habitat.nonreg.hydro<-as.data.frame(subset(metrics.hab.nonreg, select=Med:BFDur))

VOIS <-c("habitat.nonreg.topo", "habitat.nonreg.climate", "habitat.nonreg.substrate", "habitat.nonreg.landuse","habitat.nonreg.hydro") 


# random forests using party package looping through each metric and returning five most important along with their importance values



 for (ii in 1:ncol(METRICs)){   cat(ii,'...')   
        out = fut.num.month= matrix(NA,nrow=ncol(habitat.nonreg.topo),ncol=44)
        set.seed(42)
        tdata.cf <- cforest(METRICs[,ii] ~ ., data=habitat.nonreg.topo, control = cforest_unbiased(ntree = 1000)) 
        importance = as.data.frame(varimp(tdata.cf, conditional =TRUE))
        colnames(importance) = 'import'
        importance$Variable_names <- rownames(importance)
        importance <- importance[c("Variable_names","import")]
        importance_sorted <- as.data.frame(importance[order(importance$import), ])
        #importance_sorted_top5 = importance_sorted[47:51,1&2]
        out = cbind(out,importance_sorted) 
        #habitat.nonreg.sub<- habitat.nonreg[,importance_sorted[47:51,1]]
        #richALL.ct <- ctree(metrics.nonreg$RICH ~ ., data=habitat.nonreg.sub, control = ctree_control(mincriterion = 0.8)) # this makes the criterion p=0.2!
        
        }
 save(out,file=paste(data.dir,"Results/","topo_Rdata",sep='')) 
 
 for (ii in 1:ncol(METRICs)){   cat(ii,'...')   
        out = fut.num.month= matrix(NA,nrow=ncol(habitat.nonreg.climate),ncol=44)
        set.seed(42)
        tdata.cf <- cforest(METRICs[,ii] ~ ., data=habitat.nonreg.climate, control = cforest_unbiased(ntree = 1000)) 
        importance = as.data.frame(varimp(tdata.cf, conditional =TRUE))
        colnames(importance) = 'import'
        importance$Variable_names <- rownames(importance)
        importance <- importance[c("Variable_names","import")]
        importance_sorted <- as.data.frame(importance[order(importance$import), ])
        #importance_sorted_top5 = importance_sorted[47:51,1&2]
        out = cbind(out,importance_sorted) 
        #habitat.nonreg.sub<- habitat.nonreg[,importance_sorted[47:51,1]]
        #richALL.ct <- ctree(metrics.nonreg$RICH ~ ., data=habitat.nonreg.sub, control = ctree_control(mincriterion = 0.8)) # this makes the criterion p=0.2!
        
        } 
save(out,file=paste(data.dir,"Results/","climate_Rdata",sep=''))   


for (ii in 1:ncol(METRICs)){   cat(ii,'...')   
        out = fut.num.month= matrix(NA,nrow=ncol(habitat.nonreg.substrate),ncol=44)
        set.seed(42)
        tdata.cf <- cforest(METRICs[,ii] ~ ., data=habitat.nonreg.substrate, control = cforest_unbiased(ntree = 1000)) 
        importance = as.data.frame(varimp(tdata.cf, conditional =TRUE))
        colnames(importance) = 'import'
        importance$Variable_names <- rownames(importance)
        importance <- importance[c("Variable_names","import")]
        importance_sorted <- as.data.frame(importance[order(importance$import), ])
        #importance_sorted_top5 = importance_sorted[47:51,1&2]
        out = cbind(out,importance_sorted) 
        #habitat.nonreg.sub<- habitat.nonreg[,importance_sorted[47:51,1]]
        #richALL.ct <- ctree(metrics.nonreg$RICH ~ ., data=habitat.nonreg.sub, control = ctree_control(mincriterion = 0.8)) # this makes the criterion p=0.2!
        
        } 
save(out,file=paste(data.dir,"Results/","substrate_Rdata",sep=''))   


for (ii in 1:ncol(METRICs)){   cat(ii,'...')   
        out = fut.num.month= matrix(NA,nrow=ncol(habitat.nonreg.landuse),ncol=44)
        set.seed(42)
        tdata.cf <- cforest(METRICs[,ii] ~ ., data=habitat.nonreg.landuse, control = cforest_unbiased(ntree = 1000)) 
        importance = as.data.frame(varimp(tdata.cf, conditional =TRUE))
        colnames(importance) = 'import'
        importance$Variable_names <- rownames(importance)
        importance <- importance[c("Variable_names","import")]
        importance_sorted <- as.data.frame(importance[order(importance$import), ])
        #importance_sorted_top5 = importance_sorted[47:51,1&2]
        out = cbind(out,importance_sorted) 
        #habitat.nonreg.sub<- habitat.nonreg[,importance_sorted[47:51,1]]
        #richALL.ct <- ctree(metrics.nonreg$RICH ~ ., data=habitat.nonreg.sub, control = ctree_control(mincriterion = 0.8)) # this makes the criterion p=0.2!
        
        } 
save(out,file=paste(data.dir,"Results/","landuse_Rdata",sep=''))   

for (ii in 1:ncol(METRICs)){   cat(ii,'...')   
        out = fut.num.month= matrix(NA,nrow=ncol(habitat.nonreg.hydro),ncol=44)
        set.seed(42)
        tdata.cf <- cforest(METRICs[,ii] ~ ., data=habitat.nonreg.hydro, control = cforest_unbiased(ntree = 1000)) 
        importance = as.data.frame(varimp(tdata.cf, conditional =TRUE))
        colnames(importance) = 'import'
        importance$Variable_names <- rownames(importance)
        importance <- importance[c("Variable_names","import")]
        importance_sorted <- as.data.frame(importance[order(importance$import), ])
        #importance_sorted_top5 = importance_sorted[47:51,1&2]
        out = cbind(out,importance_sorted) 
        #habitat.nonreg.sub<- habitat.nonreg[,importance_sorted[47:51,1]]
        #richALL.ct <- ctree(metrics.nonreg$RICH ~ ., data=habitat.nonreg.sub, control = ctree_control(mincriterion = 0.8)) # this makes the criterion p=0.2!
        
        } 
save(out,file=paste(data.dir,"Results/","hydro_Rdata",sep=''))   






















## --------------------------------------------
## Random Forests

## Metrics are RICH, D_SPECIES, D_ALL, D_LATE,
##  BA_EARLY,  BA_LATE, D_TREE, D_SHRUB


# RICHmodel   ok.....

RICH.mtry<-tuneRF(habitat.std, metrics.nonreg$RICH, ntreeTry=1000, plot=TRUE)

RICH.rf<-randomForest(metrics.nonreg$RICH ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, 
      keep.forest=TRUE)

varImpPlot(RICH.rf, sort=TRUE, type=1)
RICH.rf

# D_SPECIES model
D_SPECIES.mtry<-tuneRF(habitat.std, metrics.nonreg$D_SPECIES, ntreeTry=1000, plot=TRUE)

D_SPECIES.rf<-randomForest(metrics.nonreg$D_SPECIES ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, 
      keep.forest=TRUE)

varImpPlot(D_SPECIES.rf, sort=TRUE, type=1)
D_SPECIES.rf

# D_ALL model  CRAP
D_ALL.mtry<-tuneRF(habitat.std, metrics.nonreg$D_ALL, ntreeTry=1000, plot=TRUE)

D_ALL.rf<-randomForest(metrics.nonreg$D_ALL ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, 
      keep.forest=TRUE)

varImpPlot(D_ALL.rf, sort=TRUE, type=1)
D_ALL.rf

# BA_ALL model CRAP
BA_ALL.mtry<-tuneRF(habitat.std, metrics.nonreg$BA_ALL, ntreeTry=1000, plot=TRUE)

BA_ALL.rf<-randomForest(metrics.nonreg$BA_ALL ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, 
      keep.forest=TRUE)

varImpPlot(BA_ALL.rf, sort=TRUE, type=1)
BA_ALL.rf

# d_EXOTIC model   CRAP
D_EXOTIC.mtry<-tuneRF(habitat.std, metrics.nonreg$D_EXOTIC, ntreeTry=1000, plot=TRUE)

D_EXOTIC.rf<-randomForest(metrics.nonreg$D_EXOTIC ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, 
      keep.forest=TRUE)

varImpPlot(D_EXOTIC.rf, sort=TRUE, type=1)
D_EXOTIC.rf
# D_NATIVE model
D_NATIVE.mtry<-tuneRF(habitat.std, metrics.nonreg$D_NATIVE, ntreeTry=1000, plot=TRUE)

D_NATIVE.rf<-randomForest(metrics.nonreg$D_NATIVE ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_NATIVE.rf, sort=TRUE, type=1)
D_NATIVE.rf

# TREEDENS model CRAP     9.73
D_TREE.mtry<-tuneRF(habitat.std, metrics.nonreg$D_TREE, ntreeTry=1000, plot=TRUE)

D_TREE.rf<-randomForest(log(metrics.nonreg$D_TREE) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_TREE.rf, sort=TRUE, type=1)
D_TREE.rf
# SHRUBDENS 
D_SHRUB.mtry<-tuneRF(habitat.std, log(metrics.nonreg$D_SHRUB+1), ntreeTry=1000, plot=TRUE)

D_SHRUB.rf<-randomForest(log(metrics.nonreg$D_SHRUB+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_SHRUB.rf, sort=TRUE, type=1)
D_SHRUB.rf
# EARLYDENS   CRAP

D_EARLY.mtry<-tuneRF(habitat.std, metrics.nonreg$D_EARLY, ntreeTry=1000, plot=TRUE)

D_EARLY.rf<-randomForest(log(metrics.nonreg$D_EARLY) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_EARLY.rf, sort=TRUE, type=1)
D_EARLY.rf

# INTERDENS CRAP 
D_INTER.mtry<-tuneRF(habitat.std, metrics.nonreg$D_INTER, ntreeTry=1000, plot=TRUE)

D_INTER.rf<-randomForest(log(metrics.nonreg$D_INTER) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_INTER.rf, sort=TRUE, type=1)
D_INTER.rf

# LATEDENS 
D_LATE.mtry<-tuneRF(habitat.std, metrics.nonreg$D_LATE, ntreeTry=1000, plot=TRUE)

D_LATE.rf<-randomForest(log(metrics.nonreg$D_LATE) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_LATE.rf, sort=TRUE, type=1)
D_LATE.rf

# BA_EXOTIC    zeros
BA_EXOTIC.mtry<-tuneRF(habitat.std, log(metrics.nonreg$BA_EXOTIC+1), ntreeTry=1000, plot=TRUE)

BA_EXOTIC.rf<-randomForest(log(metrics.nonreg$BA_EXOTIC+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_EXOTIC.rf, sort=TRUE, type=1)
BA_EXOTIC.rf
# TREEBAS  CRAP
BA_TREE.mtry<-tuneRF(habitat.std, metrics.nonreg$BA_TREE, ntreeTry=1000, plot=TRUE)

BA_TREE.rf<-randomForest(log(metrics.nonreg$BA_TREE) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_TREE.rf, sort=TRUE, type=1)
 BA_TREE.rf
# SHRUBBAS    - ZEROS!  9.73
BA_SHRUB.mtry<-tuneRF(habitat.std, log(metrics.nonreg$BA_SHRUB+1), ntreeTry=1000, plot=TRUE)

BA_SHRUB.rf<-randomForest(log(metrics.nonreg$BA_SHRUB+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_SHRUB.rf, sort=TRUE, type=1)
 BA_SHRUB.rf
# EARLYBAS    3.22
BA_EARLY.mtry<-tuneRF(habitat.std, metrics.nonreg$BA_EARLY, ntreeTry=1000, plot=TRUE)

BA_EARLY.rf<-randomForest(log(metrics.nonreg$BA_EARLY) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_EARLY.rf, sort=TRUE, type=1)
BA_EARLY.rf
# INTERBAS 
BA_INTER.mtry<-tuneRF(habitat.std, metrics.nonreg$BA_INTER, ntreeTry=1000, plot=TRUE)

BA_INTER.rf<-randomForest(log(metrics.nonreg$BA_INTER) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_INTER.rf, sort=TRUE, type=1)
BA_INTER.rf
# LATEBAS     
BA_LATE.mtry<-tuneRF(habitat.std, log(metrics.nonreg$BA_LATE+1), ntreeTry=1000, plot=TRUE)

BA_LATE.rf<-randomForest(log(metrics.nonreg$BA_LATE+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(BA_LATE.rf, sort=TRUE, type=1)
BA_LATE.rf
# TREE REGEN model 9.48
D_REGEN.mtry<-tuneRF(habitat.std, log(metrics.nonreg$D_REGEN), ntreeTry=1000, plot=TRUE)

D_REGEN.rf<-randomForest(log(metrics.nonreg$D_REGEN) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_REGEN.rf, sort=TRUE, type=1)
D_REGEN.rf

# NATIVE TREE REGEN model 
D_REGEN_NATIVE.mtry<-tuneRF(habitat.std, log(metrics.nonreg$D_REGEN_NATIVE), ntreeTry=1000, plot=TRUE)

D_REGEN_NATIVE.rf<-randomForest(log(metrics.nonreg$D_REGEN_NATIVE) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_REGEN_NATIVE.rf, sort=TRUE, type=1)
D_REGEN_NATIVE.rf

# EXOTIC TREE REGEN model 
D_REGEN_EXOTIC.mtry<-tuneRF(habitat.std, log(metrics.nonreg$D_REGEN_EXOTIC+1), ntreeTry=1000, plot=TRUE)

D_REGEN_EXOTIC.rf<-randomForest(log(metrics.nonreg$D_REGEN_EXOTIC+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_REGEN_EXOTIC.rf, sort=TRUE, type=1)
D_REGEN_EXOTIC.rf

# LOMANDRA density model
D_LOMAND.mtry<-tuneRF(habitat.std, log(metrics.nonreg$D_LOMAND+1), ntreeTry=1000, plot=TRUE)

D_LOMAND.rf<-randomForest(log(metrics.nonreg$D_LOMAND+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(D_LOMAND.rf, sort=TRUE, type=1)
D_LOMAND.rf

# EXOTIC PERCENT   CRAP
EXOTICPER.mtry<-tuneRF(habitat.std, log(metrics.nonreg$EXOTICPER+1), ntreeTry=1000, plot=TRUE)

EXOTICPER.rf<-randomForest(log(metrics.nonreg$EXOTICPER+1) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(EXOTICPER.rf, sort=TRUE, type=1)
EXOTICPER.rf

# NATIVE PERCENT
NATIVEPER.mtry<-tuneRF(habitat.std, log(metrics.hab$NATIVEPER), ntreeTry=1000, plot=TRUE)

NATIVEPER.rf<-randomForest(log(metrics.hab$NATIVEPER) ~ ., data=habitat.std, localImp=TRUE, 
      proximity=TRUE, importance=TRUE, ntree=1000, mtry=9, oob.prox=TRUE, keep.forest=TRUE)

varImpPlot(NATIVEPER.rf, sort=TRUE, type=1)



## Plot results
x11()
par(mar=c(5,4,1,1))
par(mfrow=c(3,3))
varImpPlot(RICH.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_SPECIES.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_ALL.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_ALL.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_EXOTIC.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_NATIVE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(EXOTICPER.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_EXOTIC.rf, sort=TRUE, type=1, n.var=10)

x11()
par(mar=c(5,4,1,1))
par(mfrow=c(3,3))
varImpPlot(D_EARLY.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_INTER.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_LATE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_EARLY.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_INTER.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_LATE.rf, sort=TRUE, type=1, n.var=10)


x11()
par(mar=c(5,4,1,1))
par(mfrow=c(3,3))
varImpPlot(D_SHRUB.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_TREE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_LOMAND.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_REGEN.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_SHRUB.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_TREE.rf, sort=TRUE, type=1, n.var=10)

RICH.rf$rsq
importance(RICH.rf)
D_SPECIES.rf$rsq
importance(D_SPECIES.rf)
D_NATIVE.rf$rsq
importance(D_NATIVE.rf)
BA_EXOTIC.rf$rsq
importance(BA_EXOTIC.rf)
D_INTER.rf
importance(D_INTER.rf)
D_LATE.rf
importance(D_LATE.rf)
BA_INTER.rf
importance(BA_INTER.rf)
BA_LATE.rf
importance(BA_LATE.rf)
D_SHRUB.rf
importance(D_SHRUB.rf)
D_LOMAND.rf
importance(D_LOMAND.rf)
D_REGEN_NATIVE.rf
importance(D_REGEN_NATIVE.rf)
D_REGEN_EXOTIC.rf
importance(D_REGEN_EXOTIC.rf)

# figure showing only models explaining ten percent of variance
x11()
par(mar=c(5,4,1,1))
par(mfrow=c(3,3))
varImpPlot(RICH.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_SPECIES.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_NATIVE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_EXOTIC.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_INTER.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_LATE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(BA_LATE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_SHRUB.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_LOMAND.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_REGEN_NATIVE.rf, sort=TRUE, type=1, n.var=10)
varImpPlot(D_REGEN_EXOTIC.rf, sort=TRUE, type=1, n.var=10)

# Partial dependence plots

# partial depedence plots for cv predictors

par(mfrow=c(3,3))
par(mar=c(4,4,1,1))
partialPlot(RICH.rf,habitat.std, CV, main=" ", ylab="RICH")
partialPlot(RICH.rf,habitat.std, CVDry, main=" ", ylab="RICH")
partialPlot(D_SPECIES.rf,habitat.std, CVDry, main=" ", ylab="D_SPECIES")
partialPlot(D_NATIVE.rf,habitat.std, CVDry, main=" ", ylab="D_NATIVE")
partialPlot(BA_EXOTIC.rf,habitat.std, CVWet, main=" ", ylab="BA_EXOTIC")
partialPlot(BA_EXOTIC.rf,habitat.std, CVAnnual, main=" ", ylab="BA_EXOTIC")
partialPlot(D_LATE.rf,habitat.std, CVDry, main=" ", ylab="D_LATE")
partialPlot(BA_LATE.rf,habitat.std, CVDry, main=" ", ylab="BA_LATE")
partialPlot(D_SHRUB.rf,habitat.std, CV, main=" ", ylab="D_SHRUB")
partialPlot(D_REGEN_NATIVE.rf,habitat.std, CVDry, main=" ", ylab="D_REGEN_NATIVE")
partialPlot(D_REGEN_EXOTIC.rf,habitat.std, CVAnnual, main=" ", ylab="D_REGEN_EXOTIC")
partialPlot(D_REGEN_EXOTIC.rf,habitat.std, CVWet, main=" ", ylab="D_REGEN_EXOTIC")

# partial depedence plots for average flow condition predictors

par(mfrow=c(3,3))
par(mar=c(4,4,1,1))
partialPlot(BA_EXOTIC.rf,habitat.std, LSDur, main=" ", ylab="BA_EXOTIC")
partialPlot(BA_LATE.rf,habitat.std, Med, main=" ", ylab="BA_LATE")
partialPlot(BA_LATE.rf,habitat.std, LSDis, main=" ", ylab="BA_LATE")
partialPlot(D_LOMAND.rf,habitat.std, MDFWet, main=" ", ylab="D_LOMAND")
partialPlot(D_REGEN_NATIVE.rf,habitat.std, Med, main=" ", ylab="D_REGEN_NATIVE")
partialPlot(D_REGEN_NATIVE.rf,habitat.std, LSDis, main=" ", ylab="D_REGEN_NATIVE")

# partial depedence plots for high flow predictors

par(mfrow=c(3,3))
par(mar=c(4,4,1,1))
partialPlot(D_SPECIES.rf,habitat.std, BFDis, main=" ", ylab="D_SPECIES")
partialPlot(D_INTER.rf,habitat.std, BFShear, main=" ", ylab="D_INTER")
partialPlot(D_INTER.rf,habitat.std, BFDur, main=" ", ylab="D_INTER")
partialPlot(D_LATE.rf,habitat.std, BFShear, main=" ", ylab="D_LATE")
partialPlot(D_LOMAND.rf,habitat.std, HSDis, main=" ", ylab="D_LOMAND")
partialPlot(D_REGEN_NATIVE.rf,habitat.std, BFShear, main=" ", ylab="D_REGEN_NATIVE")
partialPlot(D_SHRUB.rf,habitat.std, BFDis, main=" ", ylab="D_SHRUB")
# END
# -------------------------------------------------------
