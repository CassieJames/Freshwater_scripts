# Name: TSS.R
# Purpose: calculate True Skill Statistic (TSS) for maxent models
# Author: Christopher Tracey
# Created: 2015-03-05
# Updated: 2015-03-05
#
# Updates:
# * 2015-03-06 - added possible values for the threshold rule
#
# To Do List/Future Ideas:
# * put the threshold rule into a variable
#---------------------------------------------------------------------------------------------

# based on the script by  referenced here:
# https://groups.google.com/forum/#!msg/maxent/CUeI5xT9wTI/i3aibLdDOEYJ
# as well as the discussion located here:
# https://groups.google.com/forum/#!topic/maxent/eCgJ_0vdOb0

# "Write background predictions" has to be enabled in Maxent

# load the following packages 
library(ROCR)
library(vcd)
library(boot)

# set the working directory to where the maxent files are at
setwd("/rdsi/vol07/cciaf/SDM/models_fish/Ambassis_agassizii/output/")

list.files(pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

tss_general <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(tss_general)<-c("Replica","TSS","Bin.Prob")

lapply(listaoutput, function(nome){
  
  'Ambassis_agassizii'->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the column of predictions
  testpp <- pp[presence$Test.or.train=="test"]      # select only test points
  trainpp <- pp[presence$Test.or.train=="train"]    # select only test points
  bb <- background$logistic
  combined <- c(testpp, bb)                         # combine into a single vector
  
  read.csv("maxentResults.csv")->maxres
    
  # Set the threshold rule here
  threshold<-maxres[maxres[,1]==nome,"X10.percentile.training.presence.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==nome,"X10.percentile.training.presence.binomial.probability"]
  # other possible choices are (cut and paste as needed:
  # "Fixed.cumulative.value.1.logistic.threshold"                                       
  # "Fixed.cumulative.value.1.binomial.probability"                                     
  # "Fixed.cumulative.value.5.logistic.threshold"                                       
  # "Fixed.cumulative.value.5.binomial.probability"                                                                    
  # "Fixed.cumulative.value.10.logistic.threshold"                                                                                                                                                                        
  # "Fixed.cumulative.value.10.binomial.probability"                                                                       
  # "Minimum.training.presence.logistic.threshold"                                                                                                                                                                          
  # "Minimum.training.presence.binomial.probability"                                                                
  # "X10.percentile.training.presence.logistic.threshold"                                                                                                                                             
  # "X10.percentile.training.presence.binomial.probability"                                               
  # "Equal.training.sensitivity.and.specificity.logistic.threshold"                                                                                                   
  # "Equal.training.sensitivity.and.specificity.binomial.probability"                                 
  # "Maximum.training.sensitivity.plus.specificity.logistic.threshold"                                                                                       
  # "Maximum.training.sensitivity.plus.specificity.binomial.probability"                                     
  # "Equal.test.sensitivity.and.specificity.logistic.threshold"                                                                                                                     
  # "Equal.test.sensitivity.and.specificity.binomial.probability"                                       
  # "Maximum.test.sensitivity.plus.specificity.logistic.threshold"                                                
  # "Maximum.test.sensitivity.plus.specificity.binomial.probability"                    
  # "Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold"       
  # "Balance.training.omission..predicted.area.and.threshold.value.binomial.probability"   
  # "Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold"               
  # "Equate.entropy.of.thresholded.and.original.distributions.binomial.probability" 
  
  # Number of values greater and less than the threshold test
  sum(testpp > threshold) -> majortest
  sum(testpp < threshold) -> minortest
  
  # Number of high and low values that the threshold in the background
  sum(bb > threshold) -> majorbb
  sum(bb < threshold) -> minorbb
  
  ### Calculate sensitivity and specificity
  sensitivity <- (majortest) / (majortest+minortest)
  specificity <- (minorbb) / (majorbb+minorbb)
  
  tss <- sensitivity + specificity - 1
  
  tsssp <- as.data.frame(sp)
  tsssp[2] <- tss
  tsssp[3] <- bin.prob
  colnames(tsssp) <- c("Replica","TSS","Bin.Prob")
  
  rbind(tss_general,tsssp)->>tss_general 
})

# write the final results
write.csv(tss_general,file="_tss_general.csv")