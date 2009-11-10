
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


chopRData <- function(inputdata, chromosome) {


}

## get idea of object sizes

l.cd <- function(l = 1000000) {
  return(data.frame( ID = replicate(l, paste(sample(letters, 25), collapse ="")),
                   Chromosome = sample(1:23, l, replace = TRUE),
                   MidPoint = 1000 * runif(l)))
}
