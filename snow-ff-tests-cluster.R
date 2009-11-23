setwd("/http/tmp/kaka")

library(ff)

## load("/http/adacgh-server/test-cases/dnacopy-ok/inputData.RData")

## chromData <- ff(inputData[, 2], vmode = "ubyte",
##                                 pattern = "/http/tmp/kaka/u1")
## ul <- unlist(inputData[, -c(1, 2, 3)], use.names = FALSE)
## cghData <- ff(ul, pattern = "/http/tmp/kaka/f12",
##                             dim = c(nrow(inputData), ncol(inputData) - 3))

## ul <- unlist(inputData[, -c(1, 2, 3)], use.names = FALSE)
## cghData <- ff(ul, pattern = "/http/tmp/kaka/f13",
##                              dim = c(nrow(inputData), ncol(inputData) - 3))

## df2 <- cbind(inputData[, -c(1, 2, 3)], inputData[, -c(1, 2, 3)])
## df2[, c(4, 5, 6)] <- df2[, c(4, 5, 6)] + matrix(rnorm( 3 * nrow(df2)), ncol = 3)
## colnames(df2) <- letters[1:6]
## rownames(df2) <- NULL
## cghData2 <- as.ffdf(df2, pattern = "/http/tmp/kaka/f14")
## close(cghData2)
## save(file = "cghData2.RData", cghData2)
## close(chromData)
## close(cghData)
## save(file = "chromData.RData", chromData)
## save(file = "cghData.RData", cghData)
## rm(chromData)
## rm(cghData)
## rm(cghData2)

## rm(list = ls())





library(snowfall)
sfInit(parallel = TRUE, cpus = 10, type = "MPI")
sfClusterEval(system("hostname", intern = TRUE)) ## double check

sfLibrary(waveslim)
sfLibrary(ff)
sfLibrary(GLAD)
sfLibrary(snapCGH)
sfLibrary(ADaCGH)
sfClusterSetupRNG(type = "SPRNG")
mydir <- "/http/tmp/kaka"
sfExport("mydir")
setwd(mydir)
sfClusterEval(setwd(mydir))

## source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R")
## upload the code somehow!! sourcing, C-c C-r, whatever
## sfExport("getChromValue", "getCGHValue", "ffListOut", "internalGLAD", "sizesobj",
##          "nodeWhere", "gcmessage")

source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R")
sfClusterEval(source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R"))

uu <- pSegmentGLAD("cghData.RData", "chromData.RData")
uu2 <- pSegmentGLAD("cghData2.RData", "chromData.RData")

uu3 <- pSegmentHaarSeg("cghData2.RData", "chromData.RData")
uu4 <- pSegmentDNAcopy("cghData2.RData", "chromData.RData")



### testing existing methods

inputDataToADaCGHData(filename = "~/bzr-local-repos/adacgh-server/test-cases/dnacopy-ok/inputData.RData")

cbs <- pSegmentDNAcopy("cghData.RData", "chromData.RData")
haar <- pSegmentHaarSeg("cghData.RData", "chromData.RData")
glad <- pSegmentGLAD("cghData.RData", "chromData.RData")

t1 <- inputDataToADaCGHData(filename = "~/bzr-local-repos/adacgh-server/test-cases/dnacopy-ok/inputData.RData")

save(file = "cbs.RData", cbs)

pChromPlot(t1, "cbs.RData", "cghData.RData", "chromData.RData", "probeNames.RData")
## and note something neat: can use only a few by subsetting t1[1:5, ]


hmm1 <- pSegmentHMM("cghData.RData", "chromData.RData")
biohmm <- pSegmentBioHMM("cghData.RData", "chromData.RData", "posData.RData")

cgh1 <- pSegmentCGHseg("cghData.RData", "chromData.RData")
cgh2 <- pSegmentCGHseg("cghData.RData", "chromData.RData", mergeSegs = TRUE)

cgh3 <- pSegmentCGHseg("cghData.RData", "chromData.RData", CGHseg.thres = -0.15,
                       mergeSegs = TRUE) ## this is the one like in Wavi

wave1 <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = TRUE)

wave2 <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = TRUE,
                          minMergeDiff = 0.05, minDiff = 0.05)

wave3 <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = FALSE,
                          minMergeDiff = 0.05, minDiff = 0.25)

wave4 <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = TRUE,
                          minMergeDiff = 0.05, minDiff = 0.25)

wave5 <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = TRUE,
                          minMergeDiff = 0.05, minDiff = 0.25)



### to check same results in same data
inputDataToADaCGHData(filename = "inputData.RData.9.arrays")

cbs.9 <- pSegmentDNAcopy("cghData.RData", "chromData.RData")
haar.9 <- pSegmentHaarSeg("cghData.RData", "chromData.RData")
glad.9 <- pSegmentGLAD("cghData.RData", "chromData.RData")

## e.g., do:

round(cor(glad.9[[2]][,]), 3)
round(cor(glad.9[[1]][,]), 3)



check.rr <- function(x) {
  open(x[[1]])
  open(x[[2]])
  print(x[[1]])
  cat("\n")
  print(round(cor(x[[2]][,]), 3))
  cat("\n")
  print(round(cor(x[[1]][,]), 3))
}
  

hmm.9 <- pSegmentHMM("cghData.RData", "chromData.RData")
biohmm.9 <- pSegmentBioHMM("cghData.RData", "chromData.RData", "posData.RData")



cghseg.9.m <- pSegmentCGHseg("cghData.RData", "chromData.RData", mergeSegs = TRUE)


cghseg.9.F <- pSegmentCGHseg("cghData.RData", "chromData.RData", mergeSegs = FALSE)

cghseg.9.mad <- pSegmentCGHseg("cghData.RData", "chromData.RData", merge = "MAD")
cghseg.9.ml <- pSegmentCGHseg("cghData.RData", "chromData.RData", merge = "mergeLevels")
cghseg.9.none <- pSegmentCGHseg("cghData.RData", "chromData.RData", merge = "none")



wave.T <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = TRUE,
                          minMergeDiff = 0.05, minDiff = 0.25)
wave.F <- pSegmentWavelets("cghData.RData", "chromData.RData", mergeSegs = FALSE,
                          minMergeDiff = 0.05, minDiff = 0.25)
## the bug is in the old wavelets code then.




#####################################################

#### Testing code AND f2.R

my.f.ff <- function(ffpattern = paste(getwd(), "aa", sep = "/")) {
  vv <- ff(1:5, vmode = "ubyte",
           pattern= ffpattern)
  save(file = "vv.RData", vv)
  gc()
}



my.f.ff <- function(ffpattern = paste(getwd(), "aa", sep = "/")) {
  vv <- ff(1:5, vmode = "ubyte",
           pattern= ffpattern)
  save(file = "vv.RData", vv)
  print(2 * 2)
}


my.f2 <- function(ffpattern = paste(getwd(), "aa", sep = "/")) {
  vv <- 1:5
  save(file = "vv.RData", vv)
  print(2 * 2)
}




## it is the call to "print(gc())" as a single gc() will not cause problems
## and it is using "silent". No problem if using fork from library fork.



library(multicore)
## Problems
parallel(my.f.ff(), silent = TRUE); collect(); load("vv.RData"); open(vv); filename(vv); vv; rm(vv)

## OK
parallel(my.f.ff(), silent = FALSE); collect(); load("vv.RData"); open(vv); filename(vv); vv; rm(vv)

## OK
parallel(my.f2(), silent = TRUE); collect(); load("vv.RData"); vv; rm(vv)


library(fork)
pid <- fork(my.f.ff); wait(pid); load("vv.RData"); open(vv); filename(vv); vv; rm(vv)

## and if I use force, or sleep?
