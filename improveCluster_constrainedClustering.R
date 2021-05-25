getInitialGraph<-function(combIndxsSeed_list,L_detectInTrios)
{
 library(pracma)   
 nSeeds=length(combIndxsSeed_list)
 indxsInTrios=which(L_detectInTrios)
 combIndxsSeed_list_transform=list()
 edgeslist_seeds=array(0,c(0,2))
 indxs_seeds=c()
 
if (nSeeds>0)
   {
    for (i in 1:nSeeds)
        {
         indxs_i_tranform=indxsInTrios[combIndxsSeed_list[[i]]]
         combIndxsSeed_list_transform[[i]]=indxs_i_tranform
      
         # get all possible edges between SVs in seed i
         edgeslist_i=t(combn(indxs_i_tranform,2))
         edgeslist_seeds=rbind(edgeslist_seeds,edgeslist_i)
         indxs_seeds=c(indxs_seeds,indxs_i_tranform)
        }
    #sort
    edgeslist_seeds=t(apply(edgeslist_seeds,1,sort))
   }
 
 
 # generate selfedges for nodes not presented within seeds , and sort
 nNodes=length(L_detectInTrios)
 indxs_oneNode=setdiff((1:nNodes),indxs_seeds)
 
 if (length(indxs_oneNode)>0)
    {
     edgeslist_oneNode=cbind(indxs_oneNode,indxs_oneNode)
     edgeslist_oneNode=t(apply(edgeslist_oneNode,1,sort))
     #combine both types of edges 
     edgeslist=rbind(edgeslist_seeds,edgeslist_oneNode)    
    }
 else
    {
     edgeslist=edgeslist_seeds
    }
 
 return(list("edgeslist"=edgeslist,
             "edgeslist_seeds"=edgeslist_seeds,
             "combIndxsSeed_list_transform"=combIndxsSeed_list_transform))
}

getCanotLinkMatrix<-function(GT_numeric_i)
{
 nSVs=nrow(GT_numeric_i)   
 canotLinkMatrix=array(F,c(nSVs,nSVs))
 
 #### alternativne ###
 #nPairs=0.5*nSVs*(nSVs-1)
 #canotLinkEdges=array(NA,c(nPairs,1))
 #indx=1
 
 for (i in 1:(nSVs-1))
     {
      for (j in (i+1):nSVs)
          {
           # test self consistency
           merged_new=colSums(GT_numeric_i[c(i,j),]>0)
           if (sum(merged_new>1)>0)
              {
               canotLinkMatrix[i,j]=TRUE
              
               ### alternativne ###
               #canotLinkEdges[indx,]=c(i,j)
               #indx=indx+1
               }
          }
     }
 return(canotLinkMatrix)
 
 ### alternativne ###
 #canotLinkEdges=canotLinkEdges[1:(indx-1),]
 #return(canotLinkEdges)
}

clusteringWithConstrains<-function(initialGraphData,canotLinkMatrix,edgesWeights,maxDist,overlapType)
{
 library(pracma)
 library(prodlim)
 library(igraph)
 library(Matrix)   
      
 edgesWeights[,(1:2)]=t(apply(edgesWeights[,(1:2),drop=FALSE],1,sort))  
 
 # initialise variables
 graph_old=graph_from_edgelist(initialGraphData$edgeslist,directed=FALSE)
 clusters_old=components(graph_old)
 
 indxs_toRemove=row.match(as.data.frame(edgesWeights[,(1:2)]),as.data.frame(initialGraphData$edgeslist))
 indxs_toRemove=indxs_toRemove[(!is.na(indxs_toRemove))]
 if (length(indxs_toRemove)>0)
    {
     edgesWeights=edgesWeights[-indxs_toRemove,,drop=FALSE]
    }

 indxs_cluster12=c()

 while (nrow(edgesWeights)>0)
       {
        # get edges with actually highest value of overlap
        if (overlapType=="maxDist")
           {
            maxWeight=min(edgesWeights[,3])
            if (maxWeight>maxDist)
               {
                break
               }
           }
        else if(overlapType=="fractOverlap")
           {
            maxWeight=max(edgesWeights[,3])
            if (maxWeight<maxDist)
               {
                break
               }
           }
        
        indxsMax=which(edgesWeights[,3]==maxWeight)
        edgesToCheck=edgesWeights[indxsMax,(1:2),drop=FALSE]
        
        # test and put edges into the graph
        for (i in 1:length(indxsMax))
            {
             #### delete edges putted inside componets in previous step -- NEW
             indxs_12_edgesWeights=!((edgesWeights[,1] %in% indxs_cluster12) & (edgesWeights[,2] %in% indxs_cluster12))
             edgesWeights=edgesWeights[indxs_12_edgesWeights,,drop=FALSE] 
             ####
             
             # add new edge to old graph 
             newEdge=edgesToCheck[i,]
             
             graph_new=add_edges(graph=graph_old,edges=newEdge)
             clusters_new=components(graph_new)
             
             # find component, to which the actually putted edge belongs
             cluster_i=clusters_new$membership[newEdge[1]]
             indxs_cluster_i=which(clusters_new$membership==cluster_i)
             
             # test if any canotlink is contained within that component
             nCanotLinks=nnzero(canotLinkMatrix[indxs_cluster_i,indxs_cluster_i])
             
             # find ids of clusters being connected by newEdge
             clustersConn=clusters_old$membership[newEdge]
             indxs_cluster1=which(clusters_old$membership==clustersConn[1])
             indxs_cluster2=which(clusters_old$membership==clustersConn[2])
             indxs_cluster12=c(indxs_cluster1,indxs_cluster2) 

             if (nCanotLinks==0)
                {
                 # accept new edge (newEdge)
                 graph_old=graph_new
                 clusters_old=clusters_new
                }
             
             }
       }
 return(clusters_old)
}