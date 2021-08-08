
getIntersectedGroups<-function(gcc23)
{
 library(igraph)   

 nCombsAll=length(gcc23)
 vertexList=(1:nCombsAll)
 edgeslist=cbind(vertexList,vertexList)
 
 if (nCombsAll>1)
    {
     nPairs=0.5*nCombsAll*(nCombsAll-1)
     edgeslistPairs=array(0,c(nPairs,2))
     indx=1
     for (i in 1:(nCombsAll-1))
         {
          for (j in (i+1):nCombsAll)
              {
               ## DEPRECATED
               # if (length(intersect(gcc23[[i]],gcc23[[j]]))>0)
                 if (any(gcc23[[i]] %in% gcc23[[j]]))
                  {
                   edgeslistPairs[indx,]=c(i,j)
                   #edgeslist=rbind(edgeslist,c(i,j))
                   indx=indx+1
                  }
              }
         }
    edgeslistPairs=edgeslistPairs[1:(indx-1),,drop=F]
    if (edgeslistPairs[1,1]!=0)
       {
        edgeslist=rbind(edgeslist,edgeslistPairs)
       } 
    }

 # generate graph from edgeslist
  graph_intersect=graph_from_edgelist(edgeslist,directed=FALSE)
   
  # find weak component
  clusters=components(graph_intersect)
 
 return(clusters)
}

colapseIntersectedGroups<-function(clusters,consistConfigs,GT_svsSamples_inTrios,GT_svs_inTrios,nTriosAnalyse,gcc1_numeric)
{
 # select gcc with highest nConsist or randomly
 getGccMax_nConsist<-function(nConsist23_i)
 {
  indxs_max=which(nConsist23_i==max(nConsist23_i))
  if (length(indxs_max)>1)
     {
      indx_max=sample(indxs_max,1)
     }
  else
     {
      indx_max=indxs_max
     }    
  return(indx_max)
 }   
 
 gcc23=consistConfigs$gcc23
 nConsist23=consistConfigs$nConsist23
 
 clusterIDs=unique(clusters$membership)
 nSamples=ncol(GT_svs_inTrios)
 nCols_triosOnly=ncol(GT_svsSamples_inTrios)
 
 combIndxsSeed_list=list()
 

 for (i in 1:length(clusterIDs))
     {
      gcc23_i=gcc23[clusters$membership==clusterIDs[i]]
      nConsist23_i=nConsist23[clusters$membership==clusterIDs[i],,drop=F]
      merged=array(0,c(1,nSamples))
      merged_trios=array(0,c(1,nCols_triosOnly))
      combIndxsSeed=c()
      
      while (length(nConsist23_i)>0)
            {
             # get SVs indexes associated with ccg with highest nConsist
             indx=getGccMax_nConsist(nConsist23_i)
             combIndxs=gcc23_i[[indx]]
             
             # test self consistency
             merged_new=colSums(rbind(merged,GT_svs_inTrios[combIndxs,]>0))
             
             if (sum(merged_new>1)==0)
                {
                 ### test heritability consistency ###
                 # get GT combined with  collapsed by-product (merged_trios)   
                 GT_i=rbind(merged_trios,GT_svsSamples_inTrios[combIndxs,])
                 
                 # eval. herit. consist on GT_i
                 heritConsist=testHeritConsist(GT_i,nTriosAnalyse,gcc1_numeric)  
                
                 if (heritConsist$isConsist==TRUE)
                    {
                     combIndxsSeed=union(combIndxsSeed,combIndxs)
                     merged=merged_new
                     merged_trios_new=colSums(GT_i)
                    }
                }
            
             # remove investigated ccg + nConsist from gcc23_i and nConsist23_i
             gcc23_i[[indx]]=NULL
             nConsist23_i=nConsist23_i[-indx,,drop=F]
            }
      combIndxsSeed_list[[i]]=combIndxsSeed          
     }
 return(combIndxsSeed_list)    
}

pipeline_getDisjunctSeeds<-function(consistConfigs,gccParam,GT_svsSamples_inTrios,GT_svs_inTrios,nTriosAnalyse)
{
 gcc1_numeric=gccParam$gcc1_numeric
 if (length(consistConfigs$gcc23)==0)
    {
     combIndxsSeed_list=list()
    }
 else
    {
     # find overlapping supersets of ccgs
     clusters=getIntersectedGroups(consistConfigs$gcc23)
 
     # collapse clusters and get disjunct seeds
     combIndxsSeed_list=colapseIntersectedGroups(clusters,consistConfigs,GT_svsSamples_inTrios,GT_svs_inTrios,nTriosAnalyse,gcc1_numeric)
    }
 
 return(combIndxsSeed_list)
}
