
getHostname.System <- function (static, ...) {
    ## From the function of the same name in package R.utils (v. 1.0.1)
    ## by Henrik Bengtsson
    host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"))
    host <- host[host != ""]
    if (length(host) == 0) {
        host <- Sys.info()["nodename"]
        host <- host[host != ""]
        if (length(host) == 0) {
            host <- readLines(pipe("/usr/bin/env uname -n"))
        }
    }
    host[1]
}

the.time.with.ms <- function() {
    uu <- as.POSIXlt(Sys.time())
    return(paste(uu$hour, uu$min,
                 paste(unlist(strsplit(as.character(uu$sec), "\\.")),
                       collapse = ""), sep = ""))
}

tempdir2 <- function() {
    direxists <- TRUE
    while(direxists) {
        p1 <-  paste(getHostname.System(), round(runif(1, 1, 9999)),
                     the.time.with.ms(), sep = "_")
        p1 <- paste(tempfile(pattern = "ADaCGH_",
                             tmpdir = "."),
                    p1, sep = "_")
        if(!file.exists(p1)) direxists <- FALSE
    }
    dir.create(p1)
    return(p1)
}



load("/home/ramon/bzr-local-repos/adacgh-server/test-cases/dnacopy-ok/inputData.RData")
xcenter <- inputData[, -c(1, 2, 3), drop = FALSE]




chopData <- function(inputdata, commondata) {
  chr.change <- rle(chromosome)$lengths
  chr.start <- cumsum(chr.change)
  chr.end <- c(1, chr.start[-length(chr.start)] + 1)

  totfile <- 1
  ## using a loop, not lapply, since totfile increased
  for(arr in 1:numarrays) {
    for(chr in 1:chromnum) {
      fname <- paste("adacgh_piece", totfile, arr, chr, sep = ".")
      assign()
    }
  }
  
}

chopCommon <- function(commondata) {
  
}



x1 <- ffdf(Chromosome = ff(inputData$chromosome, vmode = "byte"), ##numeric
           MidPoint = ff(inputData$position, vmode = "double"))



## get idea of object sizes

l.cd <- function(l = 1000000) {
  return(data.frame( ID = replicate(l, paste(sample(letters, 25), collapse ="")),
                   Chromosome = sample(1:23, l, replace = TRUE),
                   MidPoint = 1000 * runif(l)))
}
<<<<<<< TREE





x1 <- ffdf(a = ff(1:1000), b = ff(11:1010))
pattern(x1) <- "/home/ramon/caca/cucu"

## close, save, then open readonly


library(snowfall)
sfInit(parallel = TRUE, 2, "MPI")

=======


## the common part is small?? Not really
## o1 <- l.cd()
## print(object.size(o1), units = "M")
## > object.size(o1)
## 96001272 bytes
## > print(object.size(o1), units = "M")
## 91.6 Mb
## > gc()
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1116966 59.7    3493455 186.6  3493455 186.6
## Vcells 7123108 54.4   18664355 142.4 18173639 138.7
## > o2 <- l.cd(2000000)
## > gc()
##            used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells  3116981 166.5    8125770 434.0  7700734 411.3
## Vcells 21123118 161.2   44573221 340.1 41123198 313.8
## > print(object.size(o2), units = "M")
## 183.1 Mb

>>>>>>> MERGE-SOURCE
