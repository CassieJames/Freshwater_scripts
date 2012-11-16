
qsub -l nodes=2 -l pmem=5gb -I

# load R

library(SDMTools); library(maptools)

#flowdata=readLines('/scratch/jc155857/graph_code/out_final_processed.csv') #read in the list of flow relationships

runoff=read.csv('/home/jc148322/NARPfreshwater/runoff.csv') #read in runoff
colnames(runoff)= c("HydroID","SEGMENTNO","runoff", "Freq", "From_Node", "Shape_Leng")
                    
streamatts = read.dbf('/home/jc246980/Janet_Stein_data/streamattributes.dbf')
Annualmean = streamatts[,2:3]

networkatts = read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')
From_node = networkatts[,c(2,11)]

runoff=runoff[,c(-4,-5,-6)]   #tidyup runoff to remove unwanted columns

################################################################################
#Create proportion file to allocate local catchment runoff

Length = networkatts[,c(2,30)]    # subset network attributes to HydroID and Shape_Leng

Runoff.temp<- merge(runoff, Length, by='HydroID')     # merge dataframes by HydroID

SegLengthTOT = aggregate(Runoff.temp$Shape_Leng, by = list(Runoff.temp$SEGMENTNO), sum)     # sum all shape lengths within SegmentNo to get total

colnames(SegLengthTOT) =c('SEGMENTNO', 'SEG.TOTAL.LENGTH')

Runoff.temp<- merge(Runoff.temp, SegLengthTOT, by='SEGMENTNO')     # merge dataframes by SEGMENTNO

runoff$seg_prop = Runoff.temp$Shape_Leng / Runoff.temp$SEG.TOTAL.LENGTH    #Create proportions to apply  and reappend to orginal runoff file



################################################################################
# identify false heads and main channels where channels bifurcate  - based on IDing all bifurcations through duplication in the From_Node


dup_nodes=networkatts$From_Node[duplicated(networkatts$From_Node)]     # gives all the duplicate AND triplicated From_Nodes   (so there will still be some duplication in this file due to triplicates!)
dup_nodes=unique(dup_nodes)                                            # list limited now to unique vales for duplicate from nodes

dupData=networkatts[which(networkatts$From_Node %in% dup_nodes),]      # subset network attributes table to dupnodes only

Segmentup=networkatts[which(networkatts$To_Node %in% dupData$From_Node),]   # subsets network data to  only those  values for To_Node that are in the unique values for duplicated From_Nodes

BIMAIN=Segmentup$NextDownID       #Identifies NextDownID from Segmentsup rem this is a hydroID!


bi_main=dupData[which(dupData$HydroID %in% BIMAIN),]                            # returns all dupData that has been identified as bi_main 
bi_sub=dupData[which(!(dupData$HydroID %in% BIMAIN)),]                          # returns all dupData that have been identified as not bi_main in the duplicated From_Node data (so must be bi-sub)
mainchannel=networkatts[which(!(networkatts$HydroID %in% dupData$HydroID)),]    # returns all networkatts that have been identified as main  (i.e.  HydroIds that are NOT below duplicated From_Nodes)

runoff["Channel_type"] <- NA #create column 

runoff$Channel_type[which(runoff$HydroID %in% bi_main$HydroID)] = c("bi_main")
runoff$Channel_type[which(runoff$HydroID %in% bi_sub$HydroID)] = c("bi_sub")
runoff$Channel_type[which(runoff$HydroID %in% mainchannel$HydroID)] = c("main")

write.csv(runoff,paste("/home/jc246980/Hydrology.trials/Aggregate_reach/Runoff.action.csv"))


################################################################################
# create partitions for accumulated flow

Runoff.part<- merge(Reach_runoff, From_node, by='HydroID')
colnames(Annualmean) =c('SEGMENTNO', 'RUNNANNMEAN')
Runoff.part<- merge(Runoff.part, Annualmean, by='SEGMENTNO')

Flow_agg_From_Node= aggregate(Runoff.part$RUNNANNMEAN, by = list(Runoff.part$From_Node), sum)      # sum flow at all duplicated From_Nodes
colnames(Flow_agg_From_Node) =c('From_Node', 'From_node_Flow')


Runoff.part<- merge(Runoff.part, Flow_agg_From_Node, by='From_Node')             # Append flow data aggregated by From_Node to data frame

runoff$acc_prop = (1+Runoff.part$Reach_runoff)/(1+Runoff.part$From_node_Flow) # create proportions  (NB where segment number is duplicated (so seg_prop <1) and a bi channel is identified use the seg_prop proportion to apportion the accumulated runoff


################################################################################
# Add from node and write out action file

runoff = merge(runoff, From_node, by='HydroID')
write.csv(runoff,paste("/home/jc246980/Hydrology.trials/Aggregate_reach/Runoff.action.csv"))

################################################################################
# validation for ignoring local run off  - check what proportion our runoff is of accumulated runoff

validation = runoff[which(runoff$Channel_type == "bi_main" |runoff$Channel_type == "bi_sub" ),] 

validation<- merge(validation, Annualmean, by='SEGMENTNO')  

validation$Val_prop =  (1+validation$runoff)/(1+validation$RUNNANNMEAN) #     

max(validation$Val_prop)   # 2236.695    !!!
median(validation$Val_prop) #0.01580761
min(validation$Val_prop)    # 6.598917e-08

quantile(validation$Val_prop,c(0,0.01,0.1,0.25,0.5,0.75,0.8,0.85,0.9,0.99,0.999,1))
#          1%          10%          25%          50%          75%          90%
#6.755342e-06 1.506598e-04 1.221365e-03 1.580761e-02 2.805427e-01 1.910540e+00
#         99%        99.9%
#2.804057e+01 1.468198e+02

tobechecked = validation[which(validation$Val_prop >1000 ),]  


# checks to make sure that all the non duplicated From_node rows are channel_type="main"
read.csv("/home/jc246980/Hydrology.trials/Aggregate_reach/Runoff.action.csv")   

fromnodes=networkatts[,c('HydroID','From_Node')]

runoff=merge(runoff,fromnodes,by='HydroID')

dup_nodes=networkatts$From_Node[duplicated(networkatts$From_Node)]    
dup_nodes=unique(dup_nodes)                                            # list limited now to unique vales for duplicate from nodes
dupData=networkatts[which(networkatts$From_Node %in% dup_nodes),]     # need to gett he hydroIDs for the duplicated nodes
nondupnodes=runoff[which(!(runoff$HydroID %in% dupData$HydroID)),]   # obtain all the non duplicated nodes = these should all be allocated main channel
unique(nondupnodes$Channel_type)            

