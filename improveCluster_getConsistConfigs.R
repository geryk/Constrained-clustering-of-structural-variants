getGccParam<-function()
{
 gcc_buildBlocks=array(NA,c(24,3))
 
 # gcc_buildBlocks2:
 gcc_buildBlocks[1,]=c(0,0,1)
 gcc_buildBlocks[2,]=c(0,0,2)
 gcc_buildBlocks[3,]=c(0,1,0)
 gcc_buildBlocks[4,]=c(0,2,0)
 gcc_buildBlocks[5,]=c(1,0,0)
 gcc_buildBlocks[6,]=c(2,0,0)
 gcc_buildBlocks[7,]=c(0,1,1)
 gcc_buildBlocks[8,]=c(0,2,2)
 gcc_buildBlocks[9,]=c(0,1,2)
 gcc_buildBlocks[10,]=c(1,1,0)
 gcc_buildBlocks[11,]=c(2,1,0)
 gcc_buildBlocks[12,]=c(1,2,0)
 gcc_buildBlocks[13,]=c(2,2,0)
 gcc_buildBlocks[14,]=c(1,0,1)
 gcc_buildBlocks[15,]=c(1,0,2)
 gcc_buildBlocks[16,]=c(2,0,2)
 gcc_buildBlocks[17,]=c(2,0,1)
 
 # missing gcc_buildBlocks1:
 gcc_buildBlocks[18,]=c(1,2,1)
 gcc_buildBlocks[19,]=c(1,1,2)
 gcc_buildBlocks[20,]=c(2,1,1)
 gcc_buildBlocks[21,]=c(2,1,2)
 gcc_buildBlocks[22,]=c(2,2,1)
 gcc_buildBlocks[23,]=c(2,2,2)
 gcc_buildBlocks[24,]=c(0,2,1)
 rownames(gcc_buildBlocks)=c("001","002","010","020","100","200","011","022","012","110","210","120","220","101","102","202","201","121","112","211","212","221","222","021")
 gcc_buildBlocks=as.data.frame(gcc_buildBlocks)
 
 gcc1_numeric=array(NA,c(14,3))
 gcc1_numeric[1,]=c(1,1,0)
 gcc1_numeric[2,]=c(1,0,1)
 gcc1_numeric[3,]=c(1,2,0)
 gcc1_numeric[4,]=c(1,0,2)
 gcc1_numeric[5,]=c(1,2,1)
 gcc1_numeric[6,]=c(1,1,2)
 gcc1_numeric[7,]=c(0,1,0)
 gcc1_numeric[8,]=c(0,0,1)
 gcc1_numeric[9,]=c(0,1,1)
 gcc1_numeric[10,]=c(2,1,1)
 gcc1_numeric[11,]=c(2,1,2)
 gcc1_numeric[12,]=c(2,2,1)
 gcc1_numeric[13,]=c(2,2,2)
 gcc1_numeric[14,]=c(1,1,1)
 gcc1_numeric=as.data.frame(gcc1_numeric)
 
 gcc1=array(NA,c(14,1))
 gcc1[1,]="110"
 gcc1[2,]="101"
 gcc1[3,]="120"
 gcc1[4,]="102"
 gcc1[5,]="121"
 gcc1[6,]="112"
 gcc1[7,]="010"
 gcc1[8,]="001"
 gcc1[9,]="011"
 gcc1[10,]="211"
 gcc1[11,]="212"
 gcc1[12,]="221"
 gcc1[13,]="222"
 gcc1[14,]="111"
 
 gcc2=array(NA,c(26,2))
 gcc2[1,]=c("001","010")
 gcc2[2,]=c("100","001")
 gcc2[3,]=c("100","020")
 gcc2[4,]=c("100","002")
 gcc2[5,]=c("120","001")
 gcc2[6,]=c("101","020")
 gcc2[7,]=c("021","100")
 gcc2[8,]=c("110","002")
 gcc2[9,]=c("102","010")
 gcc2[10,]=c("012","100")
 gcc2[11,]=c("001","010")
 gcc2[12,]=c("210","001")
 gcc2[13,]=c("201","010")
 gcc2[14,]=c("011","200")
 gcc2[15,]=c("210","002")
 gcc2[16,]=c("202","010")
 gcc2[17,]=c("012","200")
 gcc2[18,]=c("220","001")
 gcc2[19,]=c("201","020")
 gcc2[20,]=c("021","200")
 gcc2[21,]=c("220","002")
 gcc2[22,]=c("202","020")
 gcc2[23,]=c("022","200")
 gcc2[24,]=c("100","011")
 gcc2[25,]=c("010","101")
 gcc2[26,]=c("001","110")
 
 gcc3=array(NA,c(7,3))
 gcc3[1,]=c("100","020","001")
 gcc3[2,]=c("100","010","002")
 gcc3[3,]=c("200","010","001")
 gcc3[4,]=c("200","010","002")
 gcc3[5,]=c("200","020","001")
 gcc3[6,]=c("200","020","002")
 gcc3[7,]=c("100","010","001")
 
 ### gcc with at least 1 inconsistent config.
 gcc2_withInconsist=array(NA,c(20,2))
     
 gcc2_withInconsist[1,]=c("100","001")
 gcc2_withInconsist[2,]=c("100","020")
 gcc2_withInconsist[3,]=c("100","002")
 gcc2_withInconsist[4,]=c("101","020")
 gcc2_withInconsist[5,]=c("021","100")
 gcc2_withInconsist[6,]=c("110","002")
 gcc2_withInconsist[7,]=c("012","100")
 gcc2_withInconsist[8,]=c("210","001")
 gcc2_withInconsist[9,]=c("201","010")
 gcc2_withInconsist[10,]=c("011","200")
 gcc2_withInconsist[11,]=c("210","002")
 gcc2_withInconsist[12,]=c("202","010")
 gcc2_withInconsist[13,]=c("012","200")
 gcc2_withInconsist[14,]=c("220","001")
 gcc2_withInconsist[15,]=c("201","020")
 gcc2_withInconsist[16,]=c("021","200")
 gcc2_withInconsist[17,]=c("220","002")
 gcc2_withInconsist[18,]=c("202","020")
 gcc2_withInconsist[19,]=c("022","200")
 gcc2_withInconsist[20,]=c("100","011")
    
 
 
 return(list("gcc_buildBlocks"=gcc_buildBlocks,"gcc1_numeric"=gcc1_numeric,"gcc1"=gcc1,"gcc2"=gcc2,"gcc3"=gcc3,"gcc2_withInconsist"=gcc2_withInconsist))   
}

getIncidence<-function(GT_svsSamples_inTrios,nTriosAnalyse,gcc2_buildBlocks)
{
 library(prodlim)    
 nRows=nrow(GT_svsSamples_inTrios)
 indxMatrix=array(F,c(nRows,nTriosAnalyse))
 indxEnds=integer(nTriosAnalyse)
 
 indxMatricesList=list("001"=indxMatrix,"002"=indxMatrix,"010"=indxMatrix,"020"=indxMatrix,"100"=indxMatrix,
                   "200"=indxMatrix,"011"=indxMatrix,"022"=indxMatrix,"012"=indxMatrix,"022"=indxMatrix,
                   "110"=indxMatrix,"210"=indxMatrix,"120"=indxMatrix,"220"=indxMatrix,"101"=indxMatrix,
                   "102"=indxMatrix,"202"=indxMatrix,"201"=indxMatrix,
                   "121"=indxMatrix,"112"=indxMatrix,"211"=indxMatrix,"212"=indxMatrix,"221"=indxMatrix,"222"=indxMatrix,"021"=indxMatrix)
 indxEndsList=list("001"=indxEnds,"002"=indxEnds,"010"=indxEnds,"020"=indxEnds,"100"=indxEnds,
                   "200"=indxEnds,"011"=indxEnds,"022"=indxEnds,"012"=indxEnds,"022"=indxEnds,
                   "110"=indxEnds,"210"=indxEnds,"120"=indxEnds,"220"=indxEnds,"101"=indxEnds,
                   "102"=indxEnds,"202"=indxEnds,"201"=indxEnds,
                   "121"=indxEnds,"112"=indxEnds,"211"=indxEnds,"212"=indxEnds,"221"=indxEnds,"222"=indxEnds,"021"=indxEnds)
 
      for (j in 1:nTriosAnalyse) 
          {
           start_j=j*3-2
           stop_j=j*3
           # search one trio for occurences of all possible gcc2_buildBlocks
           for (i in 1:nRows)
                {
                indxmatch=row.match(GT_svsSamples_inTrios[i,(start_j:stop_j)],gcc2_buildBlocks)
                if (is.na(indxmatch)==F)
                   {
                    # find name of matched config
                    matrixName=rownames(gcc2_buildBlocks)[indxmatch]
                    
                    # increase actual row-end index for 1
                    rowIndexNew=indxEndsList[[matrixName]][j]+1
                    
                    # put new row-end index to the j-th position of vector of row-end indexes
                    indxEndsList[[matrixName]][j]=rowIndexNew
                    
                    # put index of matched SV into indxMatrix associated with matrixName
                    indxMatricesList[[matrixName]][rowIndexNew,j]=i
                   }
                }
            }
return(list("indxMatricesList"=indxMatricesList,"indxEndsList"=indxEndsList))
}

testHeritConsist<-function(GT_i,nTriosAnalyse,gcc1_numeric)
{
 nConsist=0
 isConsist=T
 nIndxs=nrow(GT_i)
 
  for (j in 1:nTriosAnalyse)
      {
       startIndx=j*3-2
       stopIndx=j*3
       GT_ij=GT_i[,(startIndx:stopIndx),drop=F]
            
       # test if merged SV is heritable consistent                 
       indxmatch=row.match(colSums(GT_ij),gcc1_numeric)
                   
       #...TODO... test if at least 1 od SVs to merge is inconsistent 
       
       nNoEmptyRows=sum(rowSums(GT_ij)>0)                                   
       if (is.na(indxmatch)==FALSE)
          {
           # test if every row of GT_ij is non-empty
           if (nNoEmptyRows==nIndxs)
              {
               nConsist=nConsist+1
               #isGccMatrix_single0[i,j]=TRUE
              }
          }
       else if (nNoEmptyRows>0)
          {
           isConsist=FALSE
           break
          }       
       }
  return(list("nConsist"=nConsist,"isConsist"=isConsist)) 
}
 
testConsistency<-function(combs,GT_svs_inTrios,GT_svsSamples_inTrios,gcc1_numeric,nTriosAnalyse)
{
 library(Matrix)   
 nRows=nrow(combs)
 nConsist=array(0,c(nRows,1))
 Lconsist=logical(nRows)
 Lconsist[]=TRUE
 #isGccMatrix_single0=array(F,c(nRows,nTriosAnalyse))

 for (i in 1:nRows)
     {
      # get SVs row indexes from i-th combination - in vector form    
      combIndxs=as.integer(combs[i,])
      
      # check self (merging) consistency
      LnoEmpty=colSums(GT_svs_inTrios[combIndxs,,drop=F])>0
      GT_svs_inTrios_noEmpty=GT_svs_inTrios[combIndxs,LnoEmpty,drop=F]
      nInconsistent=nnzero(GT_svs_inTrios_noEmpty)-ncol(GT_svs_inTrios_noEmpty)
      if (nInconsistent>0)
         {
          Lconsist[i]=FALSE
         }
      else
         {
          # merge all rows from combination i
          #merged=colSums(GT_svsSamples_inTrios[combIndxs,,drop=F])
          
          # test heritance consistrency for every trio in merged SV
          GT_i=GT_svsSamples_inTrios[combIndxs,,drop=F]
          heritConsit_i=testHeritConsist(GT_i,nTriosAnalyse,gcc1_numeric)  
          nConsist[i]=heritConsit_i$nConsist
          Lconsist[i]=heritConsit_i$isConsist
         }
         
      }
return(list("nConsist"=nConsist,"Lconsist"=Lconsist))
}

getMaxDistCombs<-function(pos,combs,nCombs,maxDist)
{
 getMaxDist<-function(pos,indxs1,indxs2)
 {
  maxDiff_start=abs(pos$start[indxs1]-pos$start[indxs2])
  maxDiff_stop=abs(pos$stop[indxs1]-pos$stop[indxs2])
  startStop=cbind(maxDiff_start,maxDiff_stop)
  mode(startStop)="numeric"
  maxDiff=rowMaxs(startStop,value=TRUE)
  return(maxDiff)
 }

 maxDiff12=getMaxDist(pos,combs[,1],combs[,2])
 if (nCombs==3)
    {
     maxDiff23=getMaxDist(pos,combs[,2],combs[,3])
     maxDiff13=getMaxDist(pos,combs[,1],combs[,3])
     maxDiff12_21_13=cbind(maxDiff12,maxDiff23,maxDiff13)
     mode(maxDiff12_21_13)="numeric"
     L_accept=rowMaxs(maxDiff12_21_13,value=TRUE)<=maxDist
    }
 else
    {
      L_accept=maxDiff12<=maxDist
    }
return(L_accept)
}

getMinOverlapCombs<-function(pos,combs,nCombs,minOverlap)
{
 getFractOverlap<-function(pos,indxs1,indxs2)
 {
  library(GenomicRanges)
  
  getGranges<-function(pos,indxs)
  {
   bedfile=data.frame(rep("chr1",length(indxs)),
                      pos$start[indxs],
                      pos$stop[indxs],
                      stringsAsFactors = F)
   colnames(bedfile)=c("chr","start","stop")
   gr=makeGRangesFromDataFrame(df=bedfile, ignore.strand=TRUE)  
  }
  
  # get granges format of genomic intervals associated with pairs of SVs
  gr1=getGranges(pos,indxs1)  
  gr2=getGranges(pos,indxs2)    
  
  n=length(indxs1)
  indxs_overlap=which(poverlaps(gr1@ranges,gr2@ranges))
 
  hits1=Hits(from=indxs_overlap,to=indxs_overlap,nLnode=n,nRnode=n,sort.by.query = F)
  
  overlaps=numeric(n)
  overlaps_nozero=overlapsRanges(gr1@ranges,gr2@ranges,hits=hits1)
  overlaps[indxs_overlap]=width(overlaps_nozero)
  
  maxWidth=width(pmax(gr1,gr2))
  
  fractOverlap=overlaps/maxWidth
  return(fractOverlap)
 }
 
 fractOverlap12=getFractOverlap(pos,combs[,1],combs[,2])
 if (nCombs==3)
    {
     fractOverlap23=getFractOverlap(pos,combs[,2],combs[,3])
     fractOverlap13=getFractOverlap(pos,combs[,1],combs[,3])
     fractOverlap12_21_13=cbind(fractOverlap12,fractOverlap23,fractOverlap13)
     mode(fractOverlap12_21_13)="numeric"
     L_accept=rowMins(fractOverlap12_21_13,value=TRUE)>=minOverlap
    }
 else
    {
      L_accept=fractOverlap12>=minOverlap
    }
 return(L_accept)
}

getCombs_singleGcc<-function(inputsIndxs,combs_checked,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,gcc1_numeric,colNamesCombs,pos_svs_inTrios,maxDist,overlapType)
{
 library(dplyr)
 library(Rfast)   
 get_combs_potential<-function(inputsIndxs,j,colNamesCombs,pos_svs_inTrios,maxDist)
 {
  indxEndsList=inputsIndxs$indxEndsList_singleGcc    
  indxMatrixList=inputsIndxs$indxMatrixList_singleGcc
  
  nCombs=length(indxEndsList)
  indxsList=list()
  for (i in 1:nCombs)
      {
       indxEnd_ij=indxEndsList[[i]][j]
       indxsList[[i]]=indxMatrixList[[i]][1:indxEnd_ij,j,drop=FALSE]
      }
  if (nCombs>1)
     {
      # get all combinations
      combs_potential=expand.grid(indxsList)

      # find and delete combinations with distance>maxDist
      if (overlapType=="maxDist")
         {
          L_accept=getMaxDistCombs(pos_svs_inTrios,combs_potential,nCombs,maxDist)
         }
      else if (overlapType=="fractOverlap")
         {
          L_accept=getMinOverlapCombs(pos_svs_inTrios,combs_potential,nCombs,maxDist)
         }
      
      combs_potential=combs_potential[L_accept,,drop=FALSE]
      
      # sort  every row for proper future comparing with other combs 
      if (nrow(combs_potential)>0)
         {
          combs_potential=t(apply(combs_potential, 1, sort))
         }
      
      combs_potential=as.data.frame(combs_potential)
     }
  else
     {
      combs_potential=as.data.frame(indxsList[[1]])
     }
  
  colnames(combs_potential)=colNamesCombs
  return(combs_potential)
 }    
  
 ### main function #### 
 nCombs=length(inputsIndxs$indxEndsList_singleGcc)
 combs_consist_single=as.data.frame(array(NA,c(0,nCombs)))
 colnames(combs_consist_single)=colNamesCombs
 nConsist=array(NA,c(0,1))
 #isGccMatrix_single=array(F,c(0,nTriosAnalyse))
 
 for (j in 1:nTriosAnalyse)
     {
      if (inputsIndxs$isPresent[j]==T)
         {
          # generate all combinations nComb SVs indexes, each drawn from one of nComb groups
          combs_potential=get_combs_potential(inputsIndxs,j,colNamesCombs,pos_svs_inTrios,maxDist)
          if (nrow(combs_potential)>0)
             {
              # remove combinations already checked for consistency
              combs_forCheck=dplyr::setdiff(combs_potential,combs_checked)
          
              if (dim(combs_forCheck)[1]>0)
                 {
                  # test consistency for evevery combination of SVs in combs2_forCheck
                  resultsConsist_j=testConsistency(combs_forCheck,GT_svs_inTrios,GT_svsSamples_inTrios,gcc1_numeric,nTriosAnalyse)

                  # fill results variables
                  Lconsist_j=resultsConsist_j$Lconsist
                  combs_consist_single=rbind(combs_consist_single,combs_forCheck[Lconsist_j,,drop=FALSE])
                  combs_checked=rbind(combs_checked,combs_forCheck)
                  nConsist=rbind(nConsist,resultsConsist_j$nConsist[Lconsist_j,,drop=F])
                  #isGccMatrix_single=rbind(isGccMatrix_single,resultsConsist_j$isGccMatrix_single0[Lconsist_j,,drop=FALSE])
                 }
             }
         }
     }
 return(list("combs_consist_single"=combs_consist_single,
             "combs_checked"=combs_checked,
             "nConsist"=nConsist))   
}
 
getCombs_gcc<-function(gccParam,gccN,resultsIncidence,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,pos_svs_inTrios,maxDist,overlapType)
{
 testBuildBlocksPresence<-function(indxEndsList,nCombs,gccN,i,nTriosAnalyse)
 {
  isPresent=logical(nTriosAnalyse)
  isPresent[]=TRUE
  for (j in 1:nCombs)
      {
       # get name of j-th configuration from singleGccN
       c_ij=gccParam[[gccN]][i,j]
       isPresent=isPresent & (indxEndsList[[c_ij]]>0)
      }
  return(isPresent)
 }
  
 getInputs<-function(indxEndsList,indxMatricesList,gccN,nCombs,i)
 {
  indxEndsList_i=list()
  indxMatrixList_i=list()
  for (j in 1:nCombs)
      {
       # get name of j-th configuration from singleGccN  
       c_ij=gccParam[[gccN]][i,j]
       indxEndsList_i[[j]]=indxEndsList[[c_ij]]
       indxMatrixList_i[[j]]=indxMatricesList[[c_ij]]
      }
  return(list("indxMatrixList_singleGcc"=indxMatrixList_i,"indxEndsList_singleGcc"=indxEndsList_i))
  }
   
 ### main function ###
 indxMatricesList=resultsIncidence$indxMatricesList   
 indxEndsList=resultsIncidence$indxEndsList  
 nCombs=ncol(gccParam[[gccN]])
 nGcc=nrow(gccParam[[gccN]])
 gcc1_numeric=gccParam$gcc1_numeric
 
 #isGccMatrix=array(F,c(0,nTriosAnalyse))
 combs_consist=as.data.frame(array(NA,c(0,nCombs)))
 combs_checked=as.data.frame(array(NA,c(0,nCombs)))
 colNamesCombs=paste("c",seq(from=1,to=nCombs,by=1),sep="")
 colnames(combs_consist)=colNamesCombs
 colnames(combs_checked)=colNamesCombs
 nConsist=array(0,c(0,1))
 for (i in 1:nGcc)
     {
      isPresent=testBuildBlocksPresence(indxEndsList,nCombs,gccN,i,nTriosAnalyse)
      # check, that at least 1 trio exists, where all of build blocks associated with examined gcc-single are present 
      if (sum(isPresent)>0)
         {
          # get inputs for  getCombs_singleGcc
          inputs_i=getInputs(indxEndsList,indxMatricesList,gccN,nCombs,i)
          inputs_i$isPresent=isPresent
          
          # find consistent combinations of gcc building blocks (resulting to gain consistent configuration after merge)
          resultsCombs=getCombs_singleGcc(inputs_i,combs_checked,nTriosAnalyse,GT_svs_inTrios,GT_svsSamples_inTrios,gcc1_numeric,colNamesCombs,pos_svs_inTrios,maxDist,overlapType)
          
          # fill results variable
          combs_consist=rbind(combs_consist,resultsCombs$combs_consist_single)
          combs_checked=resultsCombs$combs_checked
          nConsist=rbind(nConsist,resultsCombs$nConsist)
          #isGccMatrix=rbind(isGccMatrix,resultsCombs$isGccMatrix_single)
         }
     }
 return(list("combs_consist"=combs_consist,
             "combs_checked"=combs_checked,
             "nConsist"=nConsist))
}

