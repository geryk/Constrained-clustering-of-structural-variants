getMergedGT_svtype_maxDist_parallel<-function(GT_numeric_svtype,svtype,bedfile_svtype,triosVect,nTriosAnalyse,maxDist,nChunks,overlapType,pathToFunc)
{
  library(parallel)
  
  # run parallel merging
  print(paste("svtype =",svtype))
  chrNames=c(paste("chr",seq(1:22),sep=""),"chrX","chrY")
  cl=makeCluster(nChunks,outfile="")
  mergedResultsList=parSapply(cl,chrNames,getMergedGT_svtype_maxDist_chr,GT_numeric_svtype,bedfile_svtype,triosVect,nTriosAnalyse,maxDist,overlapType,pathToFunc,simplify=FALSE)
  stopCluster(cl)
  
  # merge results from individual chromosomes
  GT_merged=array(0,c(0,ncol(GT_numeric_svtype)))
  colnames(GT_merged)=colnames(GT_numeric_svtype)
  clusterVector=integer(nrow(GT_numeric_svtype))
  clusterIDs=c()
  
  maxID=0
  for (mergedResults in mergedResultsList)
      {
       GT_merged=rbind(GT_merged,mergedResults$GT_merged)
       
       # add cluster ids associated with chr to clusterVector and +maxID
       Lnozero=mergedResults$clusterVector>0
       clusterVector[Lnozero]=mergedResults$clusterVector[Lnozero]+maxID
    
       # add clusterIDs to clusterIDs vect.
       clusterIDs=c(clusterIDs,(mergedResults$clusterIDs+maxID))
       
       maxID=max(clusterIDs)
      }
  return(list("GT_merged"=GT_merged,"clusterVector"=clusterVector,"clusterIDs"=clusterIDs))
  
}

getMergedGT_svtype_maxDist_chr<-function(chr_i,GT_numeric_svtype,bedfile_svtype,triosVect,nTriosAnalyse,maxDist,overlapType,pathToFunc)
{
 library(GenomicRanges)  
  
  source(paste(pathToFunc,"dissimilarityMeasures.R",sep="/"))   
  source(paste(pathToFunc,"pipeline_improveCluster.R",sep="/"))
  source(paste(pathToFunc,"improveCluster_getConsistConfigs.R",sep="/"))
  source(paste(pathToFunc,"improveCluster_getSeeds.R",sep="/"))
  source(paste(pathToFunc,"improveCluster_constrainedClustering.R",sep="/"))
  
  mergeSVsInClusters<-function(clusters,GT_numeric_i)
  {
    nCols=ncol(GT_numeric_i)  
    GT_merged_i=array(0,c(clusters$no,nCols))
    for (i in 1:clusters$no)
    {
      L_cluster_i=clusters$membership==i
      GT_merged_i[i,]=colSums(GT_numeric_i[L_cluster_i,,drop=FALSE])
    }
    return(GT_merged_i) 
  }  
  
 # results variables  
 GT_merged=array(0,c(0,ncol(GT_numeric_svtype)))
 clusterVector=integer(nrow(GT_numeric_svtype))
 clusterIDs=c()
 
 # computations
 print(paste("maxDist=",maxDist,", chr=",chr_i))
   
 L=bedfile_svtype$chr==chr_i
      
 GT_numeric_i=GT_numeric_svtype[L,]
 bedfile_i=bedfile_svtype[L,]
 pos_i=bedfile_svtype[L,c("start","stop")]
 gr=makeGRangesFromDataFrame(df=bedfile_i, ignore.strand=TRUE)
 
 # compute dists
 if (overlapType=="maxDist")
    {
     overlapsData=getMaxDistOverlap(gr,maxDist)
     weights=overlapsData$maxDistOverlaped
    }
 else if (overlapType=="fractOverlap")
    {
     overlapsData=getMinFractOverlap(gr)
     weights=overlapsData$minFractOverapWidth
    }
   
 # get edges weights
 edgelist=cbind(queryHits(overlapsData$hits),subjectHits(overlapsData$hits)) 
 
 # find duplicated edges + self edges
 edgelist=t(apply(edgelist,1,sort))  
 L_unique=!duplicated(edgelist)
 L_notSelf=!(edgelist[,1]==edgelist[,2])
 L_selected=L_unique & L_notSelf
 edgesWeights_i=cbind(edgelist[L_selected,,drop=F],weights[L_selected])
 
 # clustering of seeds with individual SVs
 clusters=pipeline_improveCluster(GT_numeric_i,triosVect,nTriosAnalyse,edgesWeights_i,pos_i,maxDist,isClustering=TRUE,overlapType)
     
 # merge SVs from individual clusters
 GT_merged_i=mergeSVsInClusters(clusters,GT_numeric_i)
 GT_merged=rbind(GT_merged,GT_merged_i)
      
 # add new clusters ids to clusterVector
 membership_new=clusters$membership
 clusterVector[L]=membership_new
      
 # add new cluster ids to clusterIDs (cluster are merged in teh 
 #order determined by cluster ids produced by component func. from 1 to clusters$no)
 clusterIDs_new=(1:clusters$no)
 clusterIDs=c(clusterIDs,clusterIDs_new)
      
 return(list("GT_merged"=GT_merged,"clusterVector"=clusterVector,"clusterIDs"=clusterIDs))  
}
