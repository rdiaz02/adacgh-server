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




getCGHValue <- function(cghRDataName, array) {
  nmobj <- load(cghRDataName)
  open(get(nmobj, inherits = FALSE), readonly = TRUE)
  tmp <- get(nmobj, inherits = FALSE)[, array]
  close(get(nmobj, inherits = FALSE))
  return(tmp)
}

getChromValue <- function(chromRDataName) {
  nmobj <- load(chromRDataName)
  open(get(nmobj, inherits = FALSE), readonly = TRUE)
  tmp <- get(nmobj, inherits = FALSE)[]
  close(get(nmobj, inherits = FALSE))
      return(tmp)
}

getffObj <- function(RDataName, silent = FALSE) {
  nmobj <- load(RDataName, env = parent.frame())
  if(!silent) {
    cat("\n Making an assignment in the calling environment!!! \n")
    cat("We just created (or overwrote)", nmobj, "\n")
    cat("Don't forget to close", nmobj, "\n")
  }
  open(get(nmobj, inherits = FALSE, envir = parent.frame()), readonly = TRUE)
  return(nmobj)
}




ffListOut <- function(smoothedVal, stateVal) {
  pattern <- paste(getwd(), paste(sample(letters, 4), collapse = ""),
                   sep = "/")
  smoothed <- ff(smoothedVal,
                 vmode = "double",
                 pattern = pattern)
  state <- ff(stateVal,
              vmode = "short",
              pattern = pattern)
  close(smoothed)
  close(state)
  return(list(smoothed = smoothed,
              state = state))
}

internalGLAD <- function(index, cghRDataName, chromRDataName, nvalues) {
  ##  cghvalues <- getCGHval(cghRDataName, index)
  ##  chromvalues <- getChromval(chromRDataName)
  nodeWhere()
  tmpf <- list(profileValues = data.frame(
                 LogRatio = getCGHValue(cghRDataName, index),
                 PosOrder = 1:nvalues,
                 Chromosome = getChromValue(chromRDataName)))
  class(tmpf) <- "profileCGH"
  outglad <- glad.profileCGH(tmpf)
  return(ffListOut(outglad$profileValues$Smoothing,
                   outglad$profileValues$ZoneGNL))
}




pSegmentGLAD <- function(cghRDataName, chromRDataName, ...) {
  
  ## check appropriate class of objects
  
  ## stop.na.inf(x)
  ## stop.na.inf(chrom.numeric)
  ## warn.too.few.in.chrom(chrom.numeric)
  
  require("GLAD") || stop("Package not loaded: GLAD")
  
  nameCgh <- getffObj(cghRDataName, silent = TRUE)
  nameChrom <- getffObj(chromRDataName, silent = TRUE)
  arrayNames <- colnames(get(nameCgh))
  
  rle.chr <- intrle(as.integer(get(nameChrom))[])
  
  chromValues <- rle.chr$values
  nchrom <- length(chromValues)
  narrays <- ncol(get(nameCgh))
  nvalues <- nrow(get(nameCgh))
  close(get(nameCgh))
  close(get(nameChrom))
  
  outsf <- sfClusterApplyLB(1:narrays,
                            internalGLAD,
                            cghRDataName, chromRDataName, nvalues)
  
  browser()
  mysize(outsf)
  browser()
  return(outToffdf(outsf, arrayNames))
}



nodeWhere <- function() {
  fn <- paste("nodeWhere", paste(sample(letters,8), sep = "", collapse = ""),
              sep = "")
  sink(file = fn)
  date()
  cat("\n HOSTNAME IS ")
  print(system("hostname", intern = TRUE))
  sink()
}


outToffdf <- function(out, arrayNames) {
  nelem <- length(out)
  if(is.null(arrayNames))
    arrayNames <- paste("A", 1:nelem, sep = "")
  ## this is horrible, but I can't get it to work otherwise
  p1 <- paste("outSmoothed <- ffdf(",
              paste(arrayNames, " = out[[", 1:nelem, "]]$smoothed", sep = "",
                    collapse = ", "),
              ")")
  p2 <- paste("outState <- ffdf(",
              paste(arrayNames, "= out", "[[", 1:nelem, "]]$state", sep = "",
                    collapse = ", "),
              ")")
  eval(parse(text = p1))
  eval(parse(text = p2))
  colnames(outSmoothed) <- colnames(outState) <- arrayNames
  return(list(outSmoothed = outSmoothed, outState = outState))
}


mysize <- function(x) {
  cat("\n Size of object ",
      deparse(substitute(x)), ": ")
  print(object.size(x), units = "M")
}

sfExport("getChromValue", "getCGHValue", "ffListOut", "internalGLAD", "nodeWhere")

## following doesn't work.
#sfClusterEval(source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R"))

uu <- pSegmentGLAD("cghData.RData", "chromData.RData")
uu2 <- pSegmentGLAD("cghData2.RData", "chromData.RData")
