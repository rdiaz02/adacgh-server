####  Copyright (C) 2005, 2006, 2007, Ramon Diaz-Uriarte <rdiaz02@gmail.com>

#### This program is free software; you can redistribute it and/or
#### modify it under the terms of the GNU General Public License
#### as published by the Free Software Foundation; either version 2
#### of the License, or (at your option) any later version.

#### This program is distributed in the hope that it will be useful,
#### but WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#### GNU General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with this program; if not, write to the Free Software
#### Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
#### USA.

######################################################################
######################################################################
######################################################################


### Outut en formato para Dani
### FIXME: qué pasa con un solo array o un solo chrom???

### FIXME!!! change to TRUE
assign(".__ADaCGH_SERVER_APPL", FALSE)


library(ADaCGH2, verbose = FALSE)
cat("\nADaCGH2 Version :\n")
packageDescription("ADaCGH2")$Version
cat("\n\n")





##############################################
##############################################
######                              ##########
######         Error checking       ##########
######          utilities           ##########
######            and               ##########
######          other functions     ##########
######                              ##########
##############################################
##############################################


doCheckpoint <- function(num, to.save, delete.rest = TRUE) {
##    checkpoint.num.new <- num
  if(!is.null(to.save)) {
    save(file = ".RData", list = to.save, envir = .GlobalEnv)
    if(delete.rest) {
      to.delete <- setdiff(ls(envir = .GlobalEnv), to.save)
      rm(list = to.delete, envir = .GlobalEnv)
    }
  }
##    checkpoint.num <<- num
    sink("checkpoint.num")
    cat(num)
    sink()
    return(num)
}

### We will use hidden stuff from ADaCGH
#doCheckpoint <- ADaCGH:::doCheckpoint


caughtUserError.Web <- ADaCGH2:::caughtUserError.Web
caughtOurError.Web <- ADaCGH2:::caughtOurError.Web

NormalTermination <- function(){
    ADaCGH2:::snowfall.clean.quit.Web()
    status <- file("R_Status.txt", "w")
    cat("Normal termination\n", file = status)
    flush(status)
    close(status)
    cat("\n\n Normal termination\n")
    quit(save = "no", status = 0, runLast = FALSE)
}


readOptions <- function(x) {
    ### return a list, because many more indexing options
    tmp <- read.table(x, sep = "\t", stringsAsFactors = FALSE)
    l1 <- sapply(tmp[, 2], as.list)
    names(l1) <- tmp[, 1]
    return(l1)
}

checkAssign <- function(value, rangeOK, lista) {
    if(is.null(lista[[value]]))
       caughtUserError.Web(paste(value, "has no value"))
    if(lista[[value]] %in% rangeOK)
        return(lista[[value]])
    else
        caughtUserError.Web(paste(value, "has a value we do not accept"))
}

checkConvertMethodOptions <- function(method.options, options) {
  methodaCGH <- options[["method"]]
  indexopts <- which(names(method.options) == methodaCGH)
  if(length(indexopts) > 0) {
    all.method.options <- method.options[[indexopts]]
    for(mo in all.method.options) {
      if(!(mo %in% names(options)))
        caughtUserError.Web(paste(mo, " not among the options."))
      try1 <- try(options[[mo]] <- as.numeric(options[[mo]]))
      if(inherits(try1, "try-error"))
        caughtUserError.Web(paste("User failure for option ", mo))
    }
  }
  return(options)
}


acceptedIDTypes <- c('None', 'cnio', 'affy', 'clone', 'acc', 'ensembl',
                     'entrez', 'ug', 'rsrna', 'rspeptide', 'hugo')
acceptedOrganisms <- c('None', 'Hs', 'Mm', 'Rn')
acceptedMethodaCGH <- c ('Wavelets', 'DNAcopy', 'GLAD', 'HMM', 'BioHMM',
                      'CGHseg', 'HaarSeg')
methodOptions <- list('Wavelets' = c('Wave.minDiff', 'mergeRes'),
                      'CGHseg'   = c('CGHseg.s'),
                      'HaarSeg'  = c('HaarSeg.m')
                      )
acceptedColors <- colors()


## custom.out1 <- function(custom.common = custom.common,
##                         segmres = segmres,
##                         arrayNames = arrayNames) {
  
##   partC <- partB <- partA <- matrix(-9999,
##                                     nrow = nrow(custom.common),
##                                     ncol = length(arrayNames))
##   partA <- sapply(segmres[[1]], function(x) x[, 1])
##   partB <- sapply(segmres[[1]], function(x) x[, 2])
##   partC <- sapply(segmres[[1]], function(x) x[, 3])
##   colnames(partA) <- colnames(partB) <- colnames(partC) <-arrayNames
##   observed.out <- cbind(custom.common, partA)
##   rm(partA)
##   for(i in 1:3) gc()
##   segmented.out <- cbind(custom.common, partB)
##   for(i in 1:3) rm(partB)
##   calls.out <- cbind(custom.common, partC)
##   for(i in 1:3) rm(partC)
  
##   save(file = "observed.out.RData", observed.out)
##   save(file = "segmented.out.RData", segmented.out)
##   save(file = "calls.out.RData", calls.out)
  
##   write.table(file = "segmented.out.txt",
##               segmented.out, sep = "\t",
##               quote = FALSE, row.names = FALSE,
##               col.names = TRUE)
##   write.table(file = "calls.out.txt",
##               calls.out, sep = "\t",
##               quote = FALSE, row.names = FALSE,
##               col.names = TRUE)
##   system("chmod 777 calls.out.txt")
## }


    


new.custom <- function(segmresRDataName,
                       cghRDataName,
                       chromRDataName,
                       posRDataName) {
  



  close(get(nameChrom)) 

  
  narrays <- ncol(segmres[[1]])
  seqi <- seq.int(1, narrays)
  
  list.1 <- list(ProbeName = as.ff(probeNames, vmode = NULL),
                 Chr = chromData,
                 Position = posData)

  l.smoothed <- lapply(seqi,
                       function(i) segmres[[1]][[i]])
  l.calls <- lapply(seqi,
                    function(i) segmres[[2]][[i]])
  l.original <- lapply(seqi,
                       function(i) cghData[[i]])

  names(l.smoothed) <- names(l.calls) <- names(l.original) <-
    names(segmres[[1]])

  l.smoothed <- c(list.1, l.smoothed)
  l.calls <- c(list.1, l.calls)
  l.original <- c(list.1, l.original)

  smoothedffdf <- do.call("ffdf", l.smoothed)
  callsffdf <- do.call("ffdf", l.calls)
  originalffdf <- do.call("ffdf", l.original)
  
  write.table.ffdf(smoothedffdf, file = "segmented.out.txt",
                   sep = "\t", quote = FALSE)
  write.table.ffdf(callsffdf, file = "calls.out.txt",
                   sep = "\t", quote = FALSE)

  tablePos <- wrapCreateTableArrChr(cghRDataName, chromRDataName)

  
  rle.chr <- intrle(as.integer(chromData[]))
  chr.end <- cumsum(rle.chr$lengths)
  chr.start <- c(1, chr.end[-length(chr.end)] + 1)
  chromPos <- cbind(chr.start, chr.end)
  lcpos <- nrow(chromPos)

  f1 <- function(i, namein, nameout) {
    namein[ri(1, 10), ] ## un data frame ya
    df1 <- as.data.frame(original
    oname <- paste("observed.out.", i, sep = "")
    assign(oname, df1)
    save(file = paste(oname, ".RData", sep = ""),
         get(oname))
    
  
}



##############################################
##############################################
######                              ##########
######         Start execution      ##########
######                              ##########
##############################################
##############################################

if (.__ADaCGH_SERVER_APPL) {
  version
  system("date")
  system("hostname")
  cat("\nRunning\n", file = "R_Status.txt")
  hostn <- system("hostname", intern = TRUE)
  pid <- Sys.getpid()
  cat("\nPID is ", pid, "\n")
  
  startExecTime <- format(Sys.time())
  write.table(file = "pid.txt", pid,
              row.names = FALSE,
              col.names = FALSE)
  
  sink(file = "current_R_proc_info")
  cat(hostn)
  cat("  ")
  cat(pid)
  cat("\n")
  sink()

## attach pid to name in R.running.procs
  new.name1 <- unlist(strsplit(getwd(), "/"))
  new.name1 <- paste(new.name1[length(new.name1)], "@", hostn, sep = "")
  new.name <- paste("R.", new.name1, "%", pid, sep = "")
  new.name1 <- paste("R.", new.name1, sep = "")
  system(paste("mv ../../R.running.procs/", new.name1,
               " ../../R.running.procs/", new.name,
               sep = ""))
  
  checkpoint.num <- scan("checkpoint.num", what = double(0), n = 1)
} else {
  checkpoint.num <- 0
}


#######################################################
#######################################################
#######################################################
###
###       Getting and checking options
###
#######################################################
#######################################################
#######################################################


  
trythis <- try({WaviOptions <- readOptions("options.txt")})
if(inherits(trythis, "try-error"))
  caughtUserError.Web(trythis)

checkAssign("idtype", acceptedIDTypes, WaviOptions)
checkAssign("organism", acceptedOrganisms, WaviOptions)
checkAssign("method", acceptedMethodaCGH, WaviOptions)
checkAssign("colorNoChange", acceptedColors, WaviOptions)
checkAssign("colorGain", acceptedColors, WaviOptions)
checkAssign("colorLoss", acceptedColors, WaviOptions)
checkAssign("colorSmooth", acceptedColors, WaviOptions)


WaviOptions$colorsWavi <- c(WaviOptions$colorNoChange, WaviOptions$colorGain,
                        WaviOptions$colorLoss, WaviOptions$colorSmooth,
                        "black")

WaviOptions <- checkConvertMethodOptions(methodOptions, WaviOptions)    


## defaults for DNA copy and other defaults or options that will get overwritten
## if needed

## WaviOptions$DNA.undo.splits <- "prune" ## don't touch this
## WaviOptions$png.width <- 400
## WaviOptions$png.height <- 400
## WaviOptions$png.pointsize <- 10

## we do not want to mess around with options anymore
if(is.null(WaviOptions$mergeRes)) {
  if(WaviOptions$method == "DNAcopy")
    WaviOptions$mergeRes <- TRUE
  if(WaviOptions$method == "CGHseg")
    WaviOptions$mergeRes <- FALSE
}



cat("\n\n All WaviOptions are: \n")

WaviOptions


#######################################################
#######################################################
#######################################################
###
###       Read data and initial stuff
###
#######################################################
#######################################################
#######################################################

if(checkpoint.num < 1) {
  
  ## With ff and new functions. Reading data, etc, is done in a child process
  ## that does only that. That way, main process does not use a lot of RAM
  ## Verify we are doing OK killing the child, or whatever. I think we are.
  library(multicore)
  parallel(inputDataToADaCGHData(), silent = FALSE)
  tableChromArray <- collect()[[1]]
  if(inherits(tableChromArray, "try-error")) {
    caughtOurError.Web("ERROR in input data conversion")
  }

  numarrays <- max(tableChromArray$ArrayNum)
  chromnum <- max(tableChromArray$Chrom)

  cat("\n gc right before checkpoint 1 \n")
  print(gc())

  to.save <- c("numarrays", "chromnum",
               "tableChromArray",
               "WaviOptions", ".__ADaCGH_SERVER_APPL",
               "doCheckpoint", "NormalTermination",
               "caughtUserError.Web", "caughtOurError.Web")
  checkpoint.num <- doCheckpoint(1, to.save)
  
  cat("\n gc right after checkpoint 1 \n")
  gc()
}

################################################################
## MPI, LAM, etc.
## enter info into lam suffix log table


if (.__ADaCGH_SERVER_APPL) {
  trylam <- try(
                lamSESSION <- scan("lamSuffix", sep = "\t",
                                   what = "",
                                   strip.white = TRUE)
                )
  tmpDir <- getwd()
  sed.command <- paste("sed -i 's/RprocessPid\\t",
                       lamSESSION, "\\t", hostn, "/",
                       pid, "\\t",
                       lamSESSION, "\\t", hostn, "/' ",
                       "/http/adacgh-server/runs-tmp/logs/LAM_SUFFIX_Log",
                       ##                     "/http/mpi.log/LAM_SUFFIX_Log",
                       sep = "")
  
  system(sed.command)
  cat("\n\n Did sed.command ")
  cat(sed.command)
}


#######################################################
#######################################################
#######################################################
###
###       MPI and snowfall: launch
###
#######################################################
#######################################################
#######################################################

options(warn = -1)

### Launch Rmpi as late as possible with only the minimum possible slaves

library(Rmpi)

print(system("lamnodes"))
print(paste("Universe size is ", mpi.universe.size()))

usize <- min(numarrays * chromnum, mpi.universe.size())
## make sure at least two, o.w. rsprng won't work, and
## we do not want to hack my mpiInit.
print(paste("usize is", usize))

if (.__ADaCGH_SERVER_APPL) {
  usize <- max(2, usize)
  snowfallInit(universeSize = usize, exit_on_fail = TRUE)
  print("after mpiInit")
  cat("\n\nAbout to print mpiOK file\n")
  sink(file = "mpiOK")
  cat("MPI started OK\n")
  sink()
} else {
  snowfallInit(universeSize = mpi.universe.size(), exit_on_fail = FALSE)
}


#######################################################
#######################################################
#######################################################
###
###       Segmentation
###
#######################################################
#######################################################
#######################################################


if(checkpoint.num < 3) {

  trythis <- try({
    fseg <- get(paste("pSegment", WaviOptions$method, sep = ""))
    segmres <- fseg(cghRDataName = "cghData.RData",
                    chromRDataName = "chromData.RData",
                    posRDataName = "posData.RData",
                    mergeSegs = WaviOptions$mergeRes,
                    minDiff = WaviOptions$Wave.minDiff,
                    CGHseg.thres = WaviOptions$CGHseg.s,
                    HaarSeg.m = WaviOptions$HaarSeg.m)
  })
  
  if(inherits(trythis, "try-error"))
    caughtOurError.Web(trythis)
  cat("\n\n Segmentation done \n\n")
  save(segmres, file = "segmres.RData")

  ## FIXME 
  ## adacgh.server.output <- segmres[[1]]
  ## save(adacgh.server.output, file = "adacgh.server.output.RData")
  
  cat("\n gc right before checkpoint 3 \n")
  print(gc())

  to.save <- c("numarrays", "chromnum",
               "tableChromArray",
               "WaviOptions", ".__ADaCGH_SERVER_APPL",
               "doCheckpoint", "NormalTermination",
               "caughtUserError.Web", "caughtOurError.Web")
  
  checkpoint.num <- doCheckpoint(3, to.save)
   cat("\n gc right after checkpoint 3 \n")
  print(gc())
}


if(checkpoint.num < 5) {
  trythis <- try(
                 pChromPlot(outRDataName = "segmres.RData",
                            cghRDataName = "cghData.RData",
                            chromRDataName = "chromData.RData",
                            posRDataName = "posData.RData",
                            probenamesRDataName = "probeNames.RData",
                            colors = WaviOptions$colorsWavi,
                            imgheight = 350)
                 )
  if(inherits(trythis, "try-error"))
    caughtOurError.Web(trythis)
  cat("\n\n Color plotting done \n\n")
  cat("\n gc right after plotting \n")
  print(gc())

  
  ## Plots without colours
  ## As plots would get overwritten I create a new dir, etc.
  dir1 <- getwd()
  dir.create("BW")
  setwd("BW")
  print(getwd())
  sfClusterEval(setwd("BW"))
##  mpi.remote.exec(setwd("BW"))
  trythis <- try(
                 pChromPlot(outRDataName = "../segmres.RData",
                            cghRDataName = "../cghData.RData",
                            chromRDataName = "../chromData.RData",
                            posRDataName = "../posData.RData",
                            probenamesRDataName = "../probeNames.RData",
                            colors = c(rep("black", 3), "blue"),
                            imgheight = 350)
                 )
  if(inherits(trythis, "try-error"))
    caughtOurError.Web(trythis)
  files.in.BW <- dir()
  for (ffbw in files.in.BW) file.rename(ffbw, paste("BW_", ffbw, sep = ""))
  null <- file.copy(from = dir(), to = dir1)
  ##         system('mmv "*" "BW_#1"')
  ##         system('cp * ../.')
  setwd(dir1)
  sfExport("dir1")
  sfClusterEval(setwd(dir1))
##  mpi.remote.exec(setwd(dir1))
  
  cat("\n\n BW plotting done \n\n")
  cat("\n gc right after plotting \n")
  print(gc())
  NormalTermination()
}




