# Constrained-clustering-of-structural-variants
A heuristic algorithm for the identification of clusters of structural variants (SV) which corresponds to single SV with different, erroneously identified genomic positions in different samples.


# DESCRIPTION  
Set of R functions intended to SVs clustering and merging in order to reduce breakpoints uncertainty problem. The genomic position of SVs reported by many of contemporary algorithms is often inaccurate which can complicate following population analyses. Single SV shared by many individuals within population can then be recorded as many different SVs with slightly different genomic position. The presented algorithm identifies the clusters of SVs corresponding to single SV detected with different position based on constrained clustering approach. SVs from every individual cluster are then merged into single SV and resulting genotype matrix is presented as output.


# USAGE  
The R software needs to be installed (https://www.r-project.org/) together with following packages: GenomicRanges, pracma, prodlim, igraph, Matrix, dplyr, Rfast, parallel.  

Assume that the path to directory with all function from this repository is contained in the R variable: **pathToFunc**  
1)	Source file pipeline_findConsistCluster.R:  
source(paste(**pathToFunc**, pipeline_findConsistCluster.R”, sep=”/”))  
2)	Run main function (getMergedGT_svtype_maxDist_parallel) with all input variables properly set:  
results=getMergedGT_svtype_maxDist_parallel(**GT_numeric_svtype**, **svtype**, **bedfile_svtype**, **triosVect**, **nTriosAnalyse**, **maxDist**, **nChunks**, **overlapType**, **pathToFunc**)

Where:

**GT_numeric_svtype** is integer genotype matrix, were homRef is represented by 0, homAlt is represented by 2 and het is represented by 1.

**svtype** is string representing SV type for with the clustering will be performed. Allowed values are: “DEL”,”DUP”,”INS” and “INV”.

**bedfile_svtype** is data.frame with genomic positions of individual SVs in the **GT_numeric_svtype**. The names of columns must be: “chr”, “pos” and “stop”.

**triosVect** is string vector, containing all parent-child trios presented within GT_numeric_svtype. Every parents-child trio must be represented as three consecutive string names, where child’s name must be first. The names must correspond to column names of GT_numeric_svtypel. For example if we have two trios: (child1, parent1a, parent1b) and (child2, parent2a, parent2b), then: triosVect=c(“child1”, ”parent1a”, ”parent1b”, ”child2”, ”parent2a”, ”parent2b”).

**nTriosAnalyse** is number indicating how many trios will be used for analysis.

**maxDist** is maximal distance (measured by dissimilarity measure) between pair of SVs that will be associated into one cluster.

**nChunks** is number of computer cores which will be used for parallelization.

**overlapType** is string indicating type of dissimilarity measure. Allowed values are: “maxDist” and “fractOverlap”.

**pathToFunc** is string representing path to directory, where all function from this repository are contained
