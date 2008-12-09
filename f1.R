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


#### For easier debugging: we go saving results as things go along.


## rm(list = ls()) ## Just in case.

######################################################################
######################################################################
######################################################################


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


.Last <- function(){
    ## the next four lines are a way to ensure that the OS writes
    ## a file that could be immediately read by Python to see
    ## if we are done
    status <- file("R_Status.txt", "w")
    cat("Normal termination\n", file = status)
    flush(status)
    close(status)
    ##save.image()
    if (is.loaded("mpi_initialize")){ 
        if (mpi.comm.size(1) > 0){ 
        try(print("Please use mpi.close.Rslaves() to close slaves."), silent = TRUE)
        try(mpi.close.Rslaves() , silent = TRUE)
        } 
        try(print("Please use mpi.quit() to quit R"), silent = TRUE)
        cat("\n\n Normal termination\n")
        try(mpi.quit(save = "yes"), silent = TRUE)
    }
    cat("\n\n Normal termination\n")
    ## In case the CGI is not called (user kills browser)
    ## have a way to stop lam
##    try(system(paste("/http/mpi.log/killLAM.py", lamSESSION, "&")))
    try(mpi.quit(save = "yes"), silent = TRUE)
}

doCheckpoint <- function(num) {
  checkpoint.num.new <- num
  save.image()
    checkpoint.num <<- num
  sink("checkpoint.num")
    cat(num)
    sink()
}


## mpiSlaveMemClean <- function() {
##   cat("\n gc in slaves before delete \n")
##   mpi.remote.exec(gc())
##   mpi.remote.exec(rm(list = ls(env = .GlobalEnv), envir =.GlobalEnv))
##   cat("\n gc in slaves after delete \n")
##   mpi.remote.exec(gc())
  
## }

caughtUserError <- function(message) {
    message <- paste("There was a problem with something you did.\n",
                     "Check the error message, your data and options and try again.\n",
                     message)
    sink(file = "R_Error_msg.txt")
    cat(message)
    sink()
    sink(file = "R_Status.txt")
    cat("User Error\n\n")
    sink()
    quit(save = "no", status = 11, runLast = FALSE)
}



readOptions <- function(x) {
    ### return a list, because many more indexing options
    tmp <- read.table(x, sep = "\t", stringsAsFactors = FALSE)
    l1 <- sapply(tmp[, 2], as.list)
    names(l1) <- tmp[, 1]
    return(l1)
}
    

##############################################
##############################################
######                              ##########
######         Start execution      ##########
######                              ##########
##############################################
##############################################


version
system("hostname")
cat("\nRunning\n", file = "R_Status.txt")

checkpoint.num <- scan("checkpoint.num", what = double(0), n = 1)



options <- readOptions("options.txt")






##startExecTime <- format(Sys.time())

## why is this here and further down below?? FIXME
pid <- Sys.getpid()
write.table(file = "pid.txt", pid,
            row.names = FALSE,
            col.names = FALSE)


## attach pid to name in R.running.procs
hostn <- system("hostname", intern = TRUE)
new.name1 <- unlist(strsplit(getwd(), "\/"))
new.name1 <- paste(new.name1[length(new.name1)], "@", hostn, sep = "")
new.name <- paste("R.", new.name1, "%", pid, sep = "")
new.name1 <- paste("R.", new.name1, sep = "")
system(paste("mv ../../R.running.procs/", new.name1,
             " ../../R.running.procs/", new.name,
             sep = ""))

sink(file = "hostname")
cat(hostn)
sink()


### The above is no longer really needed. What follows is what buryThem2.py
##  uses
sink(file = "current_R_proc_info")
cat(hostn)
cat("   ")
cat(pid)
sink()



########################################################

########   Start MPI here to check if everything OK

#########################################################

methodaCGH <- scan("methodaCGH", what = "", n = 1)


assign(".__ADaCGH_SERVER_APPL", TRUE)
## print("testing existence of indicator")
## print(exists(".__ADaCGH_WEB_APPL"))
library(Hmisc, verbose = FALSE)
library("waveslim", verbose = FALSE) ## we will have to load ADaCGH soon,
## but we must mask certain defs. in waveslim. So load
## waveslim here
library(ADaCGH, verbose = FALSE)



## I am not sure this is really needed, since we do check this things elsewhere too,
## when we start the MPI universe from Python.

## FIXME: esto es una puta chapuza!!!! No hay razón para no paralelizar
##        las figuras de PSW!!!



## if (! ((methodaCGH == "PSW") & (checkpoint.num >= 4))) {
## we don't use MPI with PSW at the end
library(Rmpi)

## MPI_MIN_UNIVERSE_SIZE <- 15

## if (mpi.universe.size () < MPI_MIN_UNIVERSE_SIZE) {
##   cat("\n\n mpi.universe.size () < MPI_MIN_UNIVERSE_SIZE \n\n")
##   quit(save = "no", status = 11, runLast = TRUE)
## }

## try({
##   mpiInit()
##   cat("\n\nAbout to print mpiOK file\n")
##   sink(file = "mpiOK")
##   cat("MPI started OK\n")
##   sink()
## }
##    )
## } else { ## just because we need this for the controlling code
##     sink(file = "mpiOK")
##     cat("MPI started OK\n")
##     sink()
## }



startExecTime <- format(Sys.time())

pid <- Sys.getpid()
write.table(file = "pid.txt", pid,
            row.names = FALSE,
            col.names = FALSE)
trylam <- try(
              lamSESSION <- scan("lamSuffix", sep = "\t",
                                 what = "",
                                 strip.white = TRUE)
              )

#################################################################
## enter info into lam suffix log table

tmpDir <- getwd()
sed.command <- paste("sed -i 's/RprocessPid\t",
                     lamSESSION, "\t", hostn, "/",
                     pid, "\t",
                     lamSESSION, "\t", hostn, "/' ",
                     "/http/mpi.log/LAM_SUFFIX_Log",
                     sep = "")
## debugging:
sed.command

system(sed.command)







png.width = 400
png.height = 400
png.pointsize = 10
# png.family = "Helvetica"

## defaults for DNA copy
DNA.undo.splits = "prune" ## don't touch this
DNA.undo.sd = 3  ## not needed, really



warningsForUsers <- vector()

#######################################################
#######################################################
#######################################################
###
###       Read data and initial stuff
###
#######################################################
#######################################################
#######################################################



## The xdata can have either a single copy of each clone, or
## multiple. It does not matter. Averaging is done w.r.t Name in
## "positionInfo". Thus, if a single clone, all clones of a
## multi-clone gene have same weight. This can ve achieved by first
## using the preprocessor. Otherwise, each clone can have a different
## weight if diff. clones have different number of copies in the
## array.


## zz: future: allow merging by clone.then map clones, then merge by
## name and pos.

if(checkpoint.num < 1) {

idtype <- try(scan("idtype", what = "", n = 1))
organism <- try(scan("organism", what = "", n = 1))

MCR.gapAllowed <- try(scan("MCR.gapAllowed", what = double(0), n = 1))
MCR.alteredLow <- try(scan("MCR.alteredLow", what = double(0), n = 1))
MCR.alteredHigh <- try(scan("MCR.alteredHigh", what = double(0), n = 1))
MCR.recurrence <- try(scan("MCR.recurrence", what = double(0), n = 1))

Wave.minDiff <- NULL


if (methodaCGH == "Wavelets") {
    Wave.minDiff <-  scan("Wave.minDiff", what = double(0), n = 1)
    mergeRes <- Wave.merge <- scan("Wave.merge", what = "", n = 1)          
} else if (methodaCGH == "PSW") {
    PSW.nIter <- scan("PSW.nIter", what = double(0), n = 1)
    PSW.p.crit <- scan("PSW.p.crit", what = double(0), n = 1)
} else if (methodaCGH == "ACE") {
    ACE.fdr <- scan("ACE.fdr", what = double(0), n = 1)
} else if (methodaCGH == "CGHseg") {
    CGHseg.s <- scan("CGHseg.s", what = double(0), n = 1)
} 



twoFiles <- try(scan("twofiles", what = "", n = 1))
centering <- try(scan("centering", what = "", n = 1))

## positionInfo         has  name, chromosome, start, end
trypositionInfo <-
    try(
        positionInfo <- read.table("positionInfo", header = FALSE,
                                   sep = "\t",
                                   strip.white = TRUE,
                                   comment.char = "#",
                                   quote = ""))
if(class(trypositionInfo) == "try-error")
    caughtUserError(paste("The position file is not of the appropriate format\n",
                    "In case it helps this is the error we get\n",
                    trypositionInfo, sep =""))
if((ncol(positionInfo) < 4) & (twoFiles == "Two.files"))
    caughtUserError(paste("The position information file has less than four columns\n",
                    "This file MUST contain, in this order, the fields Name, Chromosome, Start, End. \n",
                          "Please see the help file."))


## index.all.missing <- apply(z, 1, function(x) !all(is.na(x)))
## the above could help to clean a data frame getting rid of all rows which are empty
## for all columns


if(ncol(positionInfo) > 4) {
    warningsForUsers <-
        c(warningsForUsers,
          paste("\nThe position information file had more than four columns.",
                "Only the first four kept.\n"))
    positionInfo <- positionInfo[, 1:4]
}



arrayNames <- scan("arrayNames", sep = "\t", what = "char", quote = "")

if(length(which(arrayNames == "")) > 1) {
    message <- paste("There are empty values for the array names\n",
                     "(or the first row in your data file).\n",
                     "That probably means you are uploading a file\n",
                     "that contains extra empty columns.\n",
                     "Please fix this problem and try again.\n",
                     "(We could try dealing with this automagically,\n",
                     "but there are some issues when guessing whether you really\n",
                     "had empty columns or legitimate columns with missing values)\n")
    caughtUserError(message)
}
if(length(arrayNames) > 0) {
    ##arrayNames <- arrayNames[-1]
    if(length(unique(arrayNames)) < length(arrayNames)) {
        dupnames <- which(duplicated(arrayNames))
        message <- paste("Array names are not unique.\n",
                         "Please change them so that they are unique.\n",
                         "The duplicated names are ", dupnames, "\n")
        caughtUserError(message)
    }
}

tryxdata <-
    try(
        xdata <- scan("covarR", what = double(0), sep = "\t")
        )
if(class(tryxdata) == "try-error")
    caughtUserError(paste("The acgh data file is not of the appropriate format\n",
                          "In case it helps this is the error we get\n",
                          tryxdata, sep =""))

xdata <- matrix(xdata, ncol = length(arrayNames), byrow = TRUE)

if(ncol(xdata) < 1)
    caughtUserError(paste("The acgh data file does not contain any data\n",
                          "(recall that the first column is only identifiers)\n"))

if(nrow(xdata) != nrow(positionInfo))
    caughtUserError(paste("Different number of genes/clones in your\n",
                          "data and position (coordinate) files.\n",
                          nrow(xdata), "genes/clones in you data file\n",
                          nrow(positionInfo), "genes/clones in you positions file.\n"))


if(ncol(xdata) != length(arrayNames)) {
    emessage <- paste("We get that the number of columns in your data (", ncol(xdata), ")\n",
                      "is different from the number of column names (", length(arrayNames), ")\n",
                      "Check for things such as '#' or '#NULL!' in the middle of your data.\n")
    caughtUserError(emessage)
}
colnames(xdata) <- arrayNames

colnames(positionInfo) <- c("name", "chromosome", "start", "end")

if(!length(arrayNames))
    cat(paste(colnames(xdata), collapse = "\t"), file = "arrayNames")

if(!(is.numeric(as.matrix(xdata)))) {
    caughtUserError("Your aCGH file contains non-numeric data. \n That is not allowed.\n")
}
if(any(is.na(xdata))) {
    caughtUserError("Your aCGH file contains missing values. \n That is not allowed.\n")
}
if(any(is.na(positionInfo))) {
    caughtUserError(paste("The position (coordinate) information contains missing values.\n",
                          "That is not allowed.\n",
                          "If you entered a single file, this means that there are missings\n",
                          "in some of the first four columns.\n"))
}
if(any(!is.numeric(as.matrix(positionInfo[, c(3, 4)])))) {
    caughtUserError(paste("Your position information (or the first four columns\n",
                          "of your single file) contains non-numeric values \n",
                          "for the start and/or end positions."))
}

## Get rid of possible chr, Chr, etc. and possible " in the chr
positionInfo$chromosome <- as.character(positionInfo$chromosome)
positionInfo$chromosome <- sub("\"", "", positionInfo$chromosome)
positionInfo$chromosome <- sub("\"", "", positionInfo$chromosome)
positionInfo$chromosome <- sub("chr", "", positionInfo$chromosome)
positionInfo$chromosome <- sub("chr ", "", positionInfo$chromosome)
positionInfo$chromosome <- sub("Chr", "", positionInfo$chromosome)
positionInfo$chromosome <- sub("Chr ", "", positionInfo$chromosome)

weird.chromos <- which(!positionInfo$chromosome %in%
                       c("X", "Y", 1:100))

if(length(weird.chromos)) {
    warningsForUsers <-
        c(warningsForUsers,
          paste("There were", length(weird.chromos),
                "clones/genes with a chromosome which was neither",
                "1 to 100 or X or Y; these have been excluded ",
                "from further analyses."))
    if (length(weird.chromos) > (dim(positionInfo)[1]/2)) {
        caughtUserError("More than half of your data have chromosomes with values that are neither an integer 1:100 or X, Y")
    }
    positionInfo <- positionInfo[-weird.chromos, ]
    xdata <- xdata[-weird.chromos, , drop = FALSE]
}

positionInfo$chromosome <- factor(positionInfo$chromosome)
positionInfo$MidPoint <- positionInfo$start +
    0.5 * (positionInfo$end - positionInfo$start)

chrom.numeric <- as.numeric(as.character(positionInfo$chromosome))
chrom.numeric[positionInfo$chromosome == "X"] <- 23
chrom.numeric[positionInfo$chromosome == "Y"] <- 24
positionInfo$chrom.numeric <- chrom.numeric
ncrom <- length(unique(chrom.numeric))
rm(chrom.numeric)
reorder <- order(positionInfo$chrom.numeric,
                 positionInfo$MidPoint,
                 positionInfo$start,
                 positionInfo$end,
                 positionInfo$name)

positionInfo <- positionInfo[reorder, ]
xdata <- xdata[reorder, , drop = FALSE]

#### Make chromosome a numeric variable
### make more sophisticated later: return X and Y in plots. zz


cat("\n gc after reading xdata \n")
gc()



#######################################################
#######################################################
#######################################################
###
###       Averaging by clones
###
#######################################################
#######################################################
#######################################################

### ordering variable

ov <- paste(positionInfo$chrom.numeric,
            positionInfo$MidPoint,
            positionInfo$name,
            sep = "*")
positionInfo$ov <- factor(ov, levels = unique(ov),
                          labels = unique(ov))
                          
### By name
xdata.merge1 <- apply(xdata, 2,
                      function(x) {
                          unlist(tapply(x,
                                        positionInfo$ov,
                                        function(z) {mean(z)}))})
positions.merge1 <- positionInfo[!duplicated(positionInfo$ov), ]

## Do we have any identical MidPos in the same chromosome??  Just to solve
## it quickly and without nasty downstream consequences, we add a runif to
## midPos. But NO further averaging.

tmp <- paste(positions.merge1$chromosome, positions.merge1$MidPoint, sep = ".")
tmp <- factor(tmp, levels = unique(tmp), labels = unique(tmp))
if (sum(duplicated(tmp))) {
    ## add a random variate, to break ties:
    positions.merge1$MidPoint[duplicated(tmp)] <-
        positions.merge1$MidPoint[duplicated(tmp)] +
            runif(sum(duplicated(tmp)))

    ## Reorder, just in case
    reorder <- order(positions.merge1$chrom.numeric,
                     positions.merge1$MidPoint,
                     positions.merge1$start,
                     positions.merge1$end,
                     positions.merge1$name)
    
    positions.merge1 <- positions.merge1[reorder, ]
    xdata <- xdata[reorder, , drop = FALSE]
}

tmp <- paste(positions.merge1$chromosome, positions.merge1$MidPoint, sep = ".")
if (sum(duplicated(tmp)))
    stopOurError("still duplicated MidPoints; shouldn't happen")


## below, we fix the distance between first obs. of each chromos
positions.merge1$DistanceClones <- c(NA, diff(positions.merge1$MidPoint))

## get ranks
obs.per.chrom <- as.vector(table(positions.merge1$chrom.numeric))
positions.merge1$rank <- unlist(sapply(obs.per.chrom,
                                    function(x) {seq(from = 1, to = x)}))

## fixing the distance of the first clone in each chromosome
positions.merge1$rank[positions.merge1$rank == 1] <- NA



#### Bail out if only a single chrom and ACE

if( (methodaCGH == "ACE") & (length(unique(positions.merge1$chrom.numeric)) == 1))
  caughtOurError(paste("There is a bug in the code that does not allow ACE",
                       "to run with only one chromosome. We are working on it."))



numarrays <- ncol(xcenter)
chromnum <- length(unique(positions.merge1$chromosome))

if(!(exists("mergeRes"))) mergeRes <- TRUE
if(mergeRes == "Yes") mergeRes <- TRUE
if(mergeRes == "No") mergeRes <- FALSE

if(!(exists("CGHseg.s"))) CGHseg.s <- NULL


doCheckpoint(1)

cat("\n gc right after checkpoint 1 \n")
gc()

}
#####################################################################
#####################################################################
options(warn = -1)



### Launch Rmpi as late as possible with only the minimum possible slaves


try({
  usize <- min(numarrays * chromnum, mpi.universe.size())
  ## make sure at least two, o.w. rsprng won't work, and
  ## we do not want to hack my mpiInit.
  usize <- max(2, usize)
  mpiInit(universeSize = usize, exit_on_fail = TRUE)
  cat("\n\nAbout to print mpiOK file\n")
  sink(file = "mpiOK")
  cat("MPI started OK\n")
  sink()
})






if(! (methodaCGH %in% c("PSW", "ACE"))) {

    if(checkpoint.num < 2) {
        common.data <- data.frame(ID = positions.merge1$name,
                                  Chromosome = positions.merge1$chromosome,
                                  Start = positions.merge1$start,
                                  End = positions.merge1$end,
                                  MidPoint = positions.merge1$MidPoint)
        doCheckpoint(2)
        cat("\n gc right after checkpoint 2 \n")
        print(gc())
    }

    if(checkpoint.num < 3) {
        
        ymax <- max(as.matrix(xcenter))
        ymin <- min(as.matrix(xcenter))
        numarrays <- ncol(xcenter)
        trythis <- try({Wave.minDiff
                        fseg <- get(paste("pSegment", methodaCGH, sep = ""))
                        segmres <- fseg(as.matrix(xcenter),
                                        chrom.numeric = positions.merge1$chrom.numeric,
                                        Pos = positions.merge1$MidPoint,
                                        mergeSegs = mergeRes,
                                        minDiff = force(Wave.minDiff),
                                        CGHseg.thres = force(CGHseg.s))
                       })
        
        if(inherits(trythis, "try-error"))
            caughtOurError(trythis)
        cat("\n\n Segmentation done \n\n")
        save(segmres, file = "segmres.RData")
        doCheckpoint(3)
        cat("\n gc right after checkpoint 3 \n")
        print(gc())
    }
    if(checkpoint.num < 4) {
        trythis <- try(doMCR(segmres$segm,
                             chrom.numeric = positions.merge1$chrom.numeric,
                             data = as.matrix(xcenter),
                             Pos = positions.merge1$MidPoint,
                             MCR.gapAllowed = MCR.gapAllowed,
                             MCR.alteredLow = MCR.alteredLow,
                             MCR.alteredHigh = MCR.alteredHigh,
                             MCR.recurrence = MCR.recurrence))
        if(inherits(trythis, "try-error"))
            caughtOurError(trythis)
        doCheckpoint(4)
        cat("\n gc right after checkpoint 4 \n")
        print(gc())
    }
    if(checkpoint.num < 5) {
        trythis <- try(
                       segmentPlot(segmres, geneNames = positions.merge1$name,
                                   chrom.numeric = positions.merge1$chrom.numeric,
                                   cghdata = NULL,
                                   arraynames = arrayNames,
                                   yminmax = c(ymin, ymax),
                                   idtype = idtype,
                                   organism = organism,
                                   numarrays = numarrays))
        if(inherits(trythis, "try-error"))
            caughtOurError(trythis)
        cat("\n\n Plotting done \n\n")
        cat("\n gc right after plotting \n")
        print(gc())
        trythis <- try(writeResults(segmres,
                                    acghdata = as.matrix(xcenter),
                                    commondata = common.data))
        if(inherits(trythis, "try-error"))
            caughtOurError(trythis)
        doCheckpoint(5)
    }
} else if(methodaCGH == "PSW") {
#######################################################
#######################################################
#######################################################
###
###            Price-Smith-Waterman
###            
###
#######################################################
#######################################################
#######################################################

    if(checkpoint.num < 2) {
        
        common.data <- data.frame(ID = positions.merge1$name,
                                Chromosome = positions.merge1$chromosome,
                                Start = positions.merge1$start,
                                End = positions.merge1$end,
                                MidPoint = positions.merge1$MidPoint)
        print("testing existence of indicator before gains")
        print(exists(".__ADaCGH_SERVER_APPL"))
        
        ## save.image()
        doCheckpoint(2)
    }

    if(checkpoint.num < 3) {
        ## Gains
        trythis <- try({
            out.gains <-
                pSegmentPSW(xcenter,
                            chrom.numeric =  positions.merge1$chrom.numeric,
                            common.data = common.data,
                            sign = +1, p.crit = PSW.p.crit,
                            nIter = PSW.nIter,
                            prec = 100,
                            name = "Gains.")
            save(file = "in.out.gains.RData", list = ls())
            cat("\n ************ done segmentation positive \n")
        })
        if(class(trythis) == "try-error")
            caughtOurError(paste("Function pSegmentPSW (positive) bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        writeResults(out.gains, commondata = common.data,
                     file = "Gains.Price.Smith.Waterman.results.txt")
        doCheckpoint(3)
    }
    if(checkpoint.num < 4) {

        print("testing existence of indicator before losses")
        print(exists(".__ADaCGH_SERVER_APPL"))
        ## Losses
        trythis <- try({
            out.losses <-
                pSegmentPSW(xcenter,
                            chrom.numeric =  positions.merge1$chrom.numeric,
                            common.data = common.data,
                            sign = -1, p.crit = PSW.p.crit,
                            nIter = PSW.nIter,
                            prec = 100,
                            name = "Losses.")
            cat("\n ************ done segmentation negative \n")
        })
        if(class(trythis) == "try-error")
            caughtOurError(paste("Function pSegmentPSW (negative) bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        writeResults(out.losses, commondata = common.data,
                     file = "Losses.Price.Smith.Waterman.results.txt")
        save(file = "PSW.RData", list = ls(all.names = TRUE))
        ADaCGH:::PSWtoPaLS()
        doCheckpoint(4)
    }
    if(checkpoint.num < 5) {
        segmentPlot(out.gains, geneNames = positions.merge1$name,
                    cghdata = xcenter,
                    arraynames = arrayNames,
                    idtype = idtype, organism = organism)
        segmentPlot(out.losses, geneNames = positions.merge1$name,
                    arraynames = arrayNames,
                    cghdata = xcenter,
                    idtype = idtype, organism = organism)
        doCheckpoint(5)
        quit(save = "yes", status = 0, runLast = TRUE)
    }
    
} else if(methodaCGH == "ACE") {

    if(checkpoint.num < 2) {

        ## zz: ugly hack: it it is a 1 dimension array, make it a vector
        ## so that the correct methods are used.
        
        if(dim(xcenter)[2] == 1) {
            one.name <- colnames(xcenter)[1]
            xcenter <- xcenter[, 1]
        }
        
        trythis <- try(
                       ACE.object <-  pSegmentACE(as.matrix(xcenter),
                                                  chrom.numeric = positions.merge1$chrom.numeric)
                       )
        if(class(trythis) == "try-error")
            caughtOurError(paste("Function pSegmentACE bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        ##save.image()
        doCheckpoint(2)
    }

    if(checkpoint.num < 3) {
        
        trythis <- try(
                       ACE.summ <- summary(ACE.object, fdr = ACE.fdr)
                       )
        if(class(trythis) == "try-error")
            caughtOurError(paste("Function summary.ACE bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        
        save(file = "ace.RData", list = ls())

        common.data <- data.frame(ID = positions.merge1$name,
                                  Chromosome = positions.merge1$chromosome,
                                  Start = positions.merge1$start,
                                  End = positions.merge1$end,
                                  MidPoint = positions.merge1$MidPoint)
        
        ## re-hack. or re-do the kuldge:
        if(is.null(dim(xcenter))) {
            xcenter <- matrix(xcenter, ncol = 1)
            colnames(xcenter) <- one.name
        }

        trythis <- try(
                       writeResults(ACE.summ,
                                    acghdata = as.matrix(xcenter),
                                    commondata = common.data,
                                    file = NULL)
                       )
        if(class(trythis) == "try-error")
            caughtOurError(paste("Function writeResults.summary.ACE.summary bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        
        
        save(file = "ace.RData", list = ls())
        
        trythis <- try(doMCR(ACE.summ$segm,
                             chrom.numeric = positions.merge1$chrom.numeric,
                             data = xcenter,
                             MCR.gapAllowed = MCR.gapAllowed,
                             MCR.alteredLow = MCR.alteredLow,
                             MCR.alteredHigh = MCR.alteredHigh,
                             MCR.recurrence = MCR.recurrence,
                             Pos = positions.merge1$MidPoint)
                       )
        if(class(trythis) == "try-error")
            caughtOurError(trythis)

        save.image()
        
        trythis <- try({
            ## The segmented plots, one per array
            segmentPlot(ACE.summ,
                        chrom.numeric = positions.merge1$chrom.numeric,
                        geneNames = positions.merge1$name,
                        cghdata = xcenter,
                        arraynames = arrayNames,
                        idtype = idtype, organism = organism)
        })
        if(class(trythis) == "try-error")
            caughtOurError(paste("Error in ACE plots  with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        save(file = "ace.RData", list = ls())
        doCheckpoint(3)
        quit(save = "yes", status = 0, runLast = TRUE)
    }
}
