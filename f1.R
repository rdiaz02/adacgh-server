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
doCheckpoint <- function(num) {
    checkpoint.num.new <- num
    save.image()
    checkpoint.num <<- num
    sink("checkpoint.num")
    cat(num)
    sink()
}

mpi.clean.quit.Web <- function() {
    if (is.loaded("mpi_initialize")){ 
        if (mpi.comm.size(1) > 0){ 
            try(mpi.close.Rslaves() , silent = TRUE)
        } 
    }
    try(mpi.exit(), silent = TRUE)
}

NormalTermination <- function(){
    mpi.clean.quit.Web()
    status <- file("R_Status.txt", "w")
    cat("Normal termination\n", file = status)
    flush(status)
    close(status)
    ##save.image()
    cat("\n\n Normal termination\n")
    ## In case the CGI is not called (user kills browser)
    ## have a way to stop lam
##    try(system(paste("/http/mpi.log/killLAM.py", lamSESSION, "&")))
##     try(mpi.quit(save = "yes"), silent = TRUE)
    quit(save = "yes", status = 0, runLast = FALSE)
}

## mpiSlaveMemClean <- function() {
##   cat("\n gc in slaves before delete \n")
##   mpi.remote.exec(gc())
##   mpi.remote.exec(rm(list = ls(env = .GlobalEnv), envir =.GlobalEnv))
##   cat("\n gc in slaves after delete \n")
##   mpi.remote.exec(gc())
  
## }

caughtOtherError.Web <- function(message) {
    mpi.clean.quit.Web()
    sink(file = "R_Error_msg.txt")
    cat(message)
    cat("\n")
    sink()
    sink(file = "R_Status.txt")
    cat("Other Error\n\n")
    sink()
    quit(save = "no", status = 11, runLast = FALSE)
}

caughtOtherPackageError.Web <- function(message) {
    mpi.clean.quit.Web()
    message <- paste("This is a known problem in a package we depend upon. ",
                     message)
    sink(file = "R_Error_msg.txt")
    cat(message)
    cat("\n")
    sink()
    sink(file = "R_Status.txt")
    cat("Other Error\n\n")
    sink()
    quit(save = "no", status = 11, runLast = FALSE)
}

caughtOurError.Web <- function(message) {
    mpi.clean.quit.Web()
    message <- paste("There was a problem with our code. Please let us know.\n", 
                     message)
    sink(file = "R_Error_msg.txt")
    cat(message)
    cat("\n")
    sink()
    sink(file = "R_Status.txt")
    cat("Our Error\n\n")
    sink()
    quit(save = "no", status = 11, runLast = FALSE)

}


caughtUserError.Web <- function(message) {
    mpi.clean.quit.Web()
    message <- paste("There was a problem with something you did.\n",
                     "Check the error message, your data and options and try again.\n",
                     message, "\n")
    sink(file = "R_Error_msg.txt")
    cat(message)
    cat("\n")
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

checkAssign <- function(value, rangeOK, lista) {
    if(is.null(lista[[value]]))
       caughtUserError.Web(paste(value, "has no value"))
    if(lista[[value]] %in% rangeOK)
        return(lista[[value]])
    else
        caughtUserError.Web(paste(value, "has a value we do not accept"))
}
    

acceptedIDTypes <- c('None', 'cnio', 'affy', 'clone', 'acc', 'ensembl',
                     'entrez', 'ug', 'rsrna', 'rspeptide', 'hugo')
acceptedOrganisms <- c('None', 'Hs', 'Mm', 'Rn')
acceptedMethodaCGH <- c ('Wavelets', 'PSW', 'DNAcopy', 'ACE', 'GLAD', 'HMM', 'BioHMM',
                      'CGHseg')
methodOptions <- list('Wavelets' = c('Wave.minDiff', 'mergeRes'),
                      'PSW'      = c('PSW.nIter', 'PSW.p.crit'),
                      'ACE'      = c('ACE.fdr'),
                      'CGHseg'   = c('CGHseg.s')
                      )

t.opt.assign <- function(nameopt, options) {
    ### Yes, we do assign into the global env.
    if(!(nameopt %in% names(options)))
        caughtUserError.Web(paste(nameopt, " not among the options."))
    try1 <- try(assign(nameopt, as.numeric(options[[nameopt]]), envir = .GlobalEnv))
    if(inherits(try1, "try-error"))
        caughtUserError.Web(paste("User failure for option ", nameopt))
}

checkMethodOptions <- function(methodaCGH, method.options, options) {
    indexopts <- which(names(method.options) == methodaCGH)
    if(length(indexopts) > 0)
        sapply(method.options[[indexopts]],
               function(x) t.opt.assign(x, options))
}



custom.out1 <- function(custom.common = custom.common,
                        segmres = segmres,
                        arrayNames = arrayNames) {
  
  partC <- partB <- partA <- matrix(-9999,
                                    nrow = nrow(custom.common),
                                    ncol = length(arrayNames))

##   for (i in 1:length(arrayNames)) {
##     partA[, i] <- segmres[[i]][, 1]
##     partB[, i] <- segmres[[i]][, 2]
##     partC[, i] <- segmres[[i]][, 3]
##   }
  partA <- sapply(segmres[[1]], function(x) x[, 1])
  partB <- sapply(segmres[[1]], function(x) x[, 2])
  partC <- sapply(segmres[[1]], function(x) x[, 3])
  
  colnames(partA) <- colnames(partB) <- colnames(partC) <-arrayNames
  observed.out <- cbind(custom.common, partA)
  rm(partA)
  for(i in 1:3) gc()
  segmented.out <- cbind(custom.common, partB)
  for(i in 1:3) rm(partB)
  calls.out <- cbind(custom.common, partC)
  for(i in 1:3) rm(partC)
  
  save(file = "observed.out.RData", observed.out)
  save(file = "segmented.out.RData", segmented.out)
  save(file = "calls.out.RData", calls.out)
  
  write.table(file = "segmented.out.txt",
              segmented.out, sep = "\t",
              quote = FALSE, row.names = FALSE,
              col.names = TRUE)
  write.table(file = "calls.out.txt",
              calls.out, sep = "\t",
              quote = FALSE, row.names = FALSE,
              col.names = TRUE)
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
hostn <- system("hostname", intern = TRUE)
pid <- Sys.getpid()
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

## defaults for DNA copy and other defaults or options that will get overwritten
## if needed
DNA.undo.splits = "prune" ## don't touch this
DNA.undo.sd = 3  ## not needed, really
Wave.minDiff <- CGHseg.s <- NA
mergeRes <- 1

trythis <- try({options <- readOptions("options.txt")})
if(inherits(trythis, "try-error"))
  caughtUserError.Web(trythis)


idtype <- checkAssign("idtype", acceptedIDTypes, options)
organism <- checkAssign("organism", acceptedOrganisms, options)
methodaCGH <- checkAssign("method", acceptedMethodaCGH, options)
checkMethodOptions(methodaCGH, methodOptions, options)    

assign(".__ADaCGH_SERVER_APPL", TRUE)


########################################################

########################################################

library(Hmisc, verbose = FALSE)
library("waveslim", verbose = FALSE) ## we will have to load ADaCGH soon,
## but we must mask certain defs. in waveslim. So load
## waveslim here
library(ADaCGH, verbose = FALSE)
library(Rmpi)

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
system(sed.command)





png.width = 400
png.height = 400
png.pointsize = 10
# png.family = "Helvetica"

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

if(checkpoint.num < 1) {

    load("inputData.RData")

    if( (methodaCGH == "ACE") & (length(unique(inputData$chromosome)) == 1))
        caughtOurError.Web(paste("There is a bug in the code that does not allow ACE",
                             "to run with only one chromosome. We are working on it."))

    if(any(is.na(inputData))) {
        caughtUserError.Web("Your aCGH file contains missing values. \n That is not allowed.\n")
    }
    xcenter <- inputData[, -c(1, 2, 3)]
    
    if(any(!(apply(xcenter, 2, is.numeric))))  {
        caughtUserError.Web("Your aCGH file contains non-numeric data. \n That is not allowed.\n")
    }
    
    cat("\n gc after reading xcenter \n")
    gc()


    ## Do we have any identical MidPos in the same chromosome??  Just to solve
    ## it quickly and without nasty downstream consequences, we add a runif to
    ## midPos. But NO further averaging.

    common.data <- data.frame(ID = inputData$ID,
                              Chromosome = inputData$chromosome, ##numeric
                              MidPoint = inputData$position)

    
    tmp <- paste(common.data$Chromosome, common.data$MidPoint, sep = ".")
    tmp <- factor(tmp, levels = unique(tmp), labels = unique(tmp))
    if (sum(duplicated(tmp))) {
        ## add a random variate, to break ties:
        common.data$MidPoint[duplicated(tmp)] <-
            common.data$MidPoint[duplicated(tmp)] +
                runif(sum(duplicated(tmp)))
        
        ## Reorder, just in case
        reorder <- order(common.data$Chromosome,
                         common.data$MidPoint,
                         common.data$ID)
        common.data <- common.data[reorder, ]
        xcenter <- xcenter[reorder, , drop = FALSE]
    }

    arrayNames <- colnames(xcenter)
    tmp <- paste(common.data$Chromosome, common.data$MidPoint, sep = ".")
    if (sum(duplicated(tmp)))
        caughtOurError.Web("still duplicated MidPoints; shouldn't happen")


    
    numarrays <- ncol(xcenter)
    chromnum <- length(unique(common.data$Chromosome))
    doCheckpoint(1)
    
    cat("\n gc right after checkpoint 1 \n")
    gc()

}

#####################################################################
#####################################################################
options(warn = -1)

### Launch Rmpi as late as possible with only the minimum possible slaves

##try({
usize <- min(numarrays * chromnum, mpi.universe.size())
## make sure at least two, o.w. rsprng won't work, and
## we do not want to hack my mpiInit.
usize <- max(2, usize)
mpiInit(universeSize = usize, exit_on_fail = TRUE)
cat("\n\nAbout to print mpiOK file\n")
sink(file = "mpiOK")
cat("MPI started OK\n")
sink()
###})


if(! (methodaCGH %in% c("PSW", "ACE"))) {
    doCheckpoint(2)
    
    if(checkpoint.num < 3) {
        
        ymax <- max(as.matrix(xcenter))
        ymin <- min(as.matrix(xcenter))
##        numarrays <- ncol(xcenter)
        trythis <- try({Wave.minDiff
                        fseg <- get(paste("pSegment", methodaCGH, sep = ""))
                        segmres <- fseg(as.matrix(xcenter),
                                        chrom.numeric = common.data$Chromosome,
                                        Pos = common.data$MidPoint,
                                        mergeSegs = mergeRes,
                                        minDiff = force(Wave.minDiff),
                                        CGHseg.thres = force(CGHseg.s))
                       })
        
        if(inherits(trythis, "try-error"))
            caughtOurError.Web(trythis)
        cat("\n\n Segmentation done \n\n")
        save(segmres, file = "segmres.RData")
        adacgh.server.output <- segmres[[1]]
        save(adacgh.server.output, file = "adacgh.server.output.RData")

        custom.common <- data.frame(ProbeName = common.data$ID,
                                    Chr = common.data$Chromosome,
                                    Position = common.data$MidPoint)

        custom.out1(custom.common, segmres, arrayNames)
        
        rm(adacgh.server.output)
        doCheckpoint(3)
        cat("\n gc right after checkpoint 3 \n")
        print(gc())
    }
    if(checkpoint.num < 4) {
        ## the MCR stuff
        doCheckpoint(4)
    }
    if(checkpoint.num < 5) {
        trythis <- try(
                       segmentPlot(segmres, geneNames = common.data$ID,
                                   chrom.numeric = common.data$Chromosome,
                                   cghdata = NULL,
                                   arraynames = arrayNames,
                                   yminmax = c(ymin, ymax),
                                   idtype = idtype,
                                   organism = organism,
                                   numarrays = numarrays))
        if(inherits(trythis, "try-error"))
            caughtOurError.Web(trythis)
        cat("\n\n Plotting done \n\n")
        cat("\n gc right after plotting \n")
        print(gc())
        ## for wavi, we don't need this
##         trythis <- try(writeResults(segmres,
##                                     acghdata = as.matrix(xcenter),
##                                     commondata = common.data))
##         if(inherits(trythis, "try-error"))
##             caughtOurError.Web(trythis)

        doCheckpoint(5)
        NormalTermination()
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
    doCheckpoint(2)

    if(checkpoint.num < 3) {
        ## Gains
        trythis <- try({
            out.gains <-
                pSegmentPSW(xcenter,
                            chrom.numeric =  common.data$Chromosome,
                            common.data = common.data,
                            sign = +1, p.crit = PSW.p.crit,
                            nIter = PSW.nIter,
                            prec = 100,
                            name = "Gains.")
            save(file = "in.out.gains.RData", list = ls())
            cat("\n ************ done segmentation positive \n")
        })
        if(class(trythis) == "try-error")
            caughtOurError.Web(paste("Function pSegmentPSW (positive) bombed unexpectedly with error",
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
                            chrom.numeric =  common.data$Chromosome,
                            common.data = common.data,
                            sign = -1, p.crit = PSW.p.crit,
                            nIter = PSW.nIter,
                            prec = 100,
                            name = "Losses.")
            cat("\n ************ done segmentation negative \n")
        })
        if(class(trythis) == "try-error")
            caughtOurError.Web(paste("Function pSegmentPSW (negative) bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        writeResults(out.losses, commondata = common.data,
                     file = "Losses.Price.Smith.Waterman.results.txt")
        save(file = "PSW.RData", list = ls(all.names = TRUE))
        ADaCGH:::PSWtoPaLS()
        doCheckpoint(4)
    }
    if(checkpoint.num < 5) {
        segmentPlot(out.gains, geneNames = common.data$ID,
                    cghdata = xcenter,
                    arraynames = arrayNames,
                    idtype = idtype, organism = organism)
        segmentPlot(out.losses, geneNames = common.data$ID,
                    arraynames = arrayNames,
                    cghdata = xcenter,
                    idtype = idtype, organism = organism)
        doCheckpoint(5)
        NormalTermination()
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
                                                  chrom.numeric = common.data$Chromosome)
                       )
        if(class(trythis) == "try-error")
            caughtOurError.Web(paste("Function pSegmentACE bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        ##save.image()
        doCheckpoint(2)
    }

    if(checkpoint.num < 3) {
        
        trythis <- try(
                       ACE.summ <- summary(ACE.object, fdr = ACE.fdr)
                       )
        if(class(trythis) == "try-error")
            caughtOurError.Web(paste("Function summary.ACE bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        
        save(file = "ace.RData", list = ls())

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
            caughtOurError.Web(paste("Function writeResults.summary.ACE.summary bombed unexpectedly with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        
        
        save(file = "ace.RData", list = ls())
        
        trythis <- try(doMCR(ACE.summ$segm,
                             chrom.numeric = common.data$Chromosome,
                             data = xcenter,
                             MCR.gapAllowed = MCR.gapAllowed,
                             MCR.alteredLow = MCR.alteredLow,
                             MCR.alteredHigh = MCR.alteredHigh,
                             MCR.recurrence = MCR.recurrence,
                             Pos = common.data$MidPoint)
                       )
        if(class(trythis) == "try-error")
            caughtOurError.Web(trythis)

        save.image()
        
        trythis <- try({
            ## The segmented plots, one per array
            segmentPlot(ACE.summ,
                        chrom.numeric = common.data$Chromosome,
                        geneNames = common.data$ID,
                        cghdata = xcenter,
                        arraynames = arrayNames,
                        idtype = idtype, organism = organism)
        })
        if(class(trythis) == "try-error")
            caughtOurError.Web(paste("Error in ACE plots  with error",
                                 trythis, ". \n Please let us know so we can fix the code."))
        save(file = "ace.RData", list = ls())
        doCheckpoint(3)
        NormalTermination()
    }
}
