pipeline_improveCluster<-function(GT_numeric_i,triosVect,nTriosAnalyse,edgesWeights_step_i,pos_i,maxDist,isClustering,overlapType)
{
 ## load functions ###

 # find SVs detected in trios used for analysis
 triosVectToAnalyse=triosVect[1:(3*nTriosAnalyse)]
 L_detectInTrios=rowSums(GT_numeric_i[,triosVectToAnalyse,drop=F])>0
 #indxs_detectInTrios=which(L_detectInTrios)
 
 # SVs detected in selected trios in all samples
 GT_svs_inTrios=GT_numeric_i[L_detectInTrios,,drop=FALSE]
 
  # start positions of SVs detected in trios
 pos_svs_inTrios=pos_i[L_detectInTrios,]
 
 # SVs detected in selected trios in samples from selected trios
 GT_svsSamples_inTrios=GT_numeric_i[L_detectInTrios,triosVectToAnalyse,drop=FALSE]

 # get gcc sets
 gccParam=getGccParam()
 
 # prepare container for results of gcc detection
 consistConfigs=list()
 
 if (nrow(GT_svs_inTrios)!=0)
    {
     # get indexes of gcc_buildBlocks within GT_svsSamples_inTrios
     resultsIncidence=getIncidence(GT_svsSamples_inTrios,nTriosAnalyse,gccParam$gcc_buildBlocks)
 
     ###########################################################
     #### get consistent configs and fill results variable #####
     consistConfigs$results_ggc1=getCombs_gcc(gccParam,"gcc1",resultsIncidence,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,pos_svs_inTrios,maxDist,overlapType)
     consistConfigs$results_ggc2=getCombs_gcc(gccParam,"gcc2",resultsIncidence,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,pos_svs_inTrios,maxDist,overlapType)
     consistConfigs$results_ggc3=getCombs_gcc(gccParam,"gcc3",resultsIncidence,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,pos_svs_inTrios,maxDist,overlapType)
     # reformat data for intersected groups colapsing
     combs_consist_2_t=as.data.frame(t(consistConfigs$results_ggc2$combs_consist))
     combs_consist_3_t=as.data.frame(t(consistConfigs$results_ggc3$combs_consist))
 
     consistConfigs$gcc23=c(as.list(combs_consist_2_t),as.list(combs_consist_3_t))
     consistConfigs$nConsist23=rbind(consistConfigs$results_ggc2$nConsist,consistConfigs$results_ggc3$nConsist)
    }
 else
    {
     consistConfigs$gcc23=c()
    }
 ###########################################################
 ##### get disjunct seeds for clustering ###################
 combIndxsSeed_list=pipeline_getDisjunctSeeds(consistConfigs,gccParam,GT_svsSamples_inTrios,GT_svs_inTrios,nTriosAnalyse)
 
 if (isClustering==TRUE)
    {
     ###########################################################
     #### cluster SVs with canot-link constrains, where seeds represents initial state
     # create cliques from seeds in order to make initial graph representation
     initialGraphData=getInitialGraph(combIndxsSeed_list,L_detectInTrios)
     print("initial graph data ... done")
     # generate canotlink edges representing pairs of Svs which are self inconsistent
     canotLinkMatrix=getCanotLinkMatrix(GT_numeric_i)
     print("canotLink matrix ... done")
     #cluster SVs (together with seed cliques) with canotlink constrains
     clusters_final=clusteringWithConstrains(initialGraphData,canotLinkMatrix,edgesWeights_step_i,maxDist,overlapType)
     results=clusters_final
    }
 else
    {
     results=combIndxsSeed_list 
    }
 return(results)
}



profilePipeline<-function()
{
library(profvis)
   
profvis({
clusters_final=pipeline_improveCluster(GT_numeric_i,triosVect,nTriosAnalyse,edgesWeights_step_i)
})
}