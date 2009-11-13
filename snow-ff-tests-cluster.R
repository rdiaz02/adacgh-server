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

sfLibrary(ff)
sfLibrary(GLAD)
sfClusterSetupRNG(type = "SPRNG")
mydir <- "/http/tmp/kaka"
sfExport("mydir")
setwd(mydir)
sfClusterEval(setwd(mydir))

## source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R")


## upload the code somehow!! sourcing, C-c C-r, whatever



sfExport("getChromValue", "getCGHValue", "ffListOut", "internalGLAD", "sizesobj",
         "nodeWhere", "gcmessage")

## following doesn't work.
#sfClusterEval(source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R"))

uu <- pSegmentGLAD("cghData.RData", "chromData.RData")
uu2 <- pSegmentGLAD("cghData2.RData", "chromData.RData")
