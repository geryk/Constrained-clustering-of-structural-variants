getMinFractOverlap<-function(gr)
{
 library(GenomicRanges)
 
 # find overlaps for every pair of intervals
 hits=findOverlaps(gr, gr)   
 overlaps=overlapsRanges(gr@ranges, gr@ranges,hits=hits)   
 
 # compute minimal overlap fraction for every pair of intervals
 gr_width=width(gr)
 maxWidthOverlaps=pmax(gr_width[queryHits(hits)],gr_width[subjectHits(hits)])
 minFractOverapWidth=width(overlaps)/maxWidthOverlaps
 
 return(list("hits"=hits,"minFractOverapWidth"=minFractOverapWidth))
}

getMaxDistOverlap<-function(gr,maxDist)
{
 library(GenomicRanges)
 library(Rfast)   
 # find overlaps for every pair of intervals
 hits=findOverlaps(gr, gr,type="equal",maxgap=maxDist)    
 
 # gets maxDist values for every overlaping pair
 starts_hits1=gr@ranges@start[hits@from]
 starts_hits2=gr@ranges@start[hits@to]
 ends_hits1=starts_hits1+gr@ranges@width[hits@from]-1
 ends_hits2=starts_hits2+gr@ranges@width[hits@to]-1
 
 diff_starts=abs(starts_hits1-starts_hits2)
 diff_ends=abs(ends_hits1-ends_hits2)
 maxDistOverlaped=rowMaxs(cbind(diff_starts,diff_ends),value=TRUE)
 
 return(list("hits"=hits,"maxDistOverlaped"=maxDistOverlaped))
}