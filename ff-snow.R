## LAM-MPI universe booted, etc.
## 4 nodes, each in a different machine


library(ff)
library(snowfall)

## Names for dirs and files
mydir <- "/http/tmp"
fn1 <- "t1"
fn2 <- "t2"
fn3 <- "t3"

fullname1 <- paste(mydir, fn1, sep = "/")
fullname2 <- paste(mydir, fn2, sep = "/")
fullname3 <- paste(mydir, fn3, sep = "/")


sfInit(parallel = TRUE, cpus = 4, type = "MPI")
sfLibrary(ff)
sfClusterSetupRNG(type = "SPRNG")
sfExport("mydir")
setwd(mydir)
sfClusterEval(setwd(mydir))

## To know where things happen
sfClusterApply(1:4, function(i) assign("nodenum", i, env = .GlobalEnv))



##   Getting and setting values of ff object in slaves
x1 <- ff(1:60, dim = c(4, 15), filename = fullname1)
close(x1)
save(file = "x1.RData", x1)

## Acessing values; read-only
sfClusterEval(load("x1.RData"))
sfClusterEval(open(x1, readonly = TRUE))
sfClusterApplyLB(1:15, function(i) {list(nodenum = nodenum, values = x1[, i])})
sfClusterEval(close(x1))



##### I think it is OK to change in place if stored as ffdf:
### each column is a different file

## a) Create ffdf in master, send to nodes, modify there

## I use pattern here, and not latter, to avoid 'Invalid cross-device link'
## if R binary and filename are in different disks

x2 <- ffdf(c1 = ff(length = 4, vmode = "double", pattern = fullname2),
           c2 = ff(length = 4, vmode = "double", pattern = fullname2),
           c3 = ff(length = 4, vmode = "double", pattern = fullname2),
           c4 = ff(length = 4, vmode = "double", pattern = fullname2),
           c5 = ff(length = 4, vmode = "double", pattern = fullname2),
           c4 = ff(length = 4, vmode = "double", pattern = fullname2),
           c7 = ff(length = 4, vmode = "double", pattern = fullname2),
           c8 = ff(length = 4, vmode = "double", pattern = fullname2),
           c9 = ff(length = 4, vmode = "double", pattern = fullname2),
           c10 = ff(length = 4, vmode = "double", pattern = fullname2),
           c11 = ff(length = 4, vmode = "double", pattern = fullname2),
           c12 = ff(length = 4, vmode = "double", pattern = fullname2),
           c13 = ff(length = 4, vmode = "double", pattern = fullname2),
           c14 = ff(length = 4, vmode = "double", pattern = fullname2),
           c15 = ff(length = 4, vmode = "double", pattern = fullname2))

close(x2)
save(file = "x2.RData", x2)

sfClusterEval(load("x2.RData"))
sfClusterEval(open(x2))
## The following is, of course, non-deterministic. Just to see it
## working.
sfClusterApplyLB(1:15, function(i) x2[, i] <- rep(nodenum, 4))
sfClusterEval(close(x2))

rm(x2)
load("x2.RData")
open(x2)
x2[, ] ## changes are stored



## b) Create ff object in nodes, and put together as ffdf in master
##    Note that we pass the object back as the return of sfClusterApplyLB
##    In contrast, in a) we do not pass anything back from
##    sfClusterApplyLB, but leave it in the disk.

sfExport("fullname3")

createAndClose <- function(i) {
  nameobj <- paste("ffpiece", i, sep = "")
  assign(nameobj,
         ff(rep(nodenum, 4), pattern = fullname3))
  close(get(nameobj))
  return(get(nameobj))
}


list.ff <- sfClusterApplyLB(1:15, createAndClose)

## put together ffdf
## x3 <- ffdf(list.ff[[1]], list.ff[[2]], list.ff[[3]]) ## etc ...

eval(parse(text = paste("x3 <- ffdf(", paste("list.ff[[", 1:15, "]]",
             sep = "", collapse = ", "), ")")))

open(x3)
x3


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


### how large are ffdf objects?


common.data <- data.frame(ID = inputData$ID,
                          Chromosome = inputData$chromosome, ##numeric
                          MidPoint = inputData$position)
xcenter <- inputData[, -c(1, 2, 3), drop = FALSE]
print(object.size(xcenter), units = "M") ## 235
print(object.size(common.data), units = "M") ## 82

## place, in here, the checking functions for chrom and data.
## when converting to ff
## Call it "checkAndConverttoff()
##



unix.time({
xmv <- as.vector(data.matrix(xcenter))
xff <- ff(xmv, filename = "/home/ramon/caca/f11", dim = c(nrow(xcenter), ncol(xcenter)))
}) # 4.4

unix.time({
ul <- unlist(xcenter, use.names = FALSE)
xff2 <- ff(ul, filename = "/home/ramon/caca/f12", dim = c(nrow(xcenter), ncol(xcenter))) 
}) # 2.09


print(object.size(xff), units = "M") ## 0
print(object.size(xff2), units = "M") ## 0


cd.ffdf <- as.ffdf(common.data, pattern = "/home/ramon/caca/f13") ## it works!
print(object.size(cd.ffdf), units = "M") ## 63




### create a large place holder

c1 = ff(length = 4, vmode = "double", pattern = fullname2),

numelements <- 2000000
numarrays <- 30
fullname2 <- "/home/ramon/caca/f14"

## let it rename the columns
eval(parse(text =
           paste("holder <- ffdf(", 
                 paste(rep(paste("V = ff(length = ", numelements,
                                 ", vmode = 'double', pattern = fullname2)"),
                           numarrays),
                       collapse = ", "),
                 ")")
           )
     )

print(object.size(holder), units = "M") # 0.1


### Create in slaves and prepare ffdf in master

sfInit(parallel = TRUE, cpus = 4, type = "MPI")
sfLibrary(ff)
sfClusterSetupRNG(type = "SPRNG")
mydir <- "/home/ramon/caca"
sfExport("mydir")
setwd(mydir)
sfClusterEval(setwd(mydir))



createAndClose2 <- function(i) {
  nameobj <- paste("ffpiece", i, sep = "")
  assign(nameobj,
         ff(rnorm(2000000), pattern = "/home/ramon/caca/hhindi"))
  close(get(nameobj))
  return(get(nameobj))
}

list.ff1 <- sfClusterApplyLB(1:30, createAndClose2)


createAndClose3 <- function(i) {
  tmp <- ff(rnorm(2000000), pattern = "/home/ramon/caca/ggindiv")
  close(tmp)
  return(tmp)
}
list.ff2 <- sfClusterApplyLB(1:30, createAndClose3)



ffdf.from.list <- function(name, list1) {
  ne <- length(get(list1))
  assign(name,
         eval(parse(text = paste("ffdf(",
               paste("V = ", list1, "[[", 1:ne, "]]",
                     sep = "", collapse = ", "), ")")
             )),
         envir = .GlobalEnv)
}

ffdf.from.list("ffdf1", "list.ff1")
ffdf.from.list("ffdf2", "list.ff2")
open(ffdf1)
open(ffdf2)

print(object.size(ffdf1), units = "M") ## 0.1
print(object.size(ffdf2), units = "M") ## 0.1








##### More checks on coversion, etc

## setwd("/home/ramon/bzr-local-repos/adacgh-server/test-cases/dnacopy-ok")

df1 <- as.ffdf(inputData[, c(1, 2, 3)], pattern = "/home/ramon/caca/f22")
df2 <- as.ffdf(inputData[, c(2, 3)], pattern = "/home/ramon/caca/f23")



### Do checks on number of values of chromosome; if more than 255, use ushort.
df3 <- ffdf(chromosome = ff(inputData[, 2], vmode = "ubyte",
              pattern = "/home/ramon/caca/u1"),
            position = ff(inputData[, 3], vmode = "double",
              pattern = "/home/ramon/caca/u1"))

IDs <- inputData[, 1] ## no point using an ffdf




##### Doing some testing


load("/home/ramon/bzr-local-repos/adacgh-server/test-cases/dnacopy-ok/inputData.RData")

chromData <- ff(inputData[, 2], vmode = "ubyte",
                pattern = "/home/ramon/caca/u1")
ul <- unlist(inputData[, -c(1, 2, 3)], use.names = FALSE)
cghData <- ff(ul, pattern = "/home/ramon/caca/f12",
              dim = c(nrow(inputData), ncol(inputData) - 3)) 
close(chromData)
close(cghData)
save(file = "chromData.RData", chromData)
save(file = "cghData.RData", cghData)
rm(chromData)
rm(cghData)


## create it also from a data frame (will think about speedier ways later)
## and make it larger
df2 <- cbind(inputData[, -c(1, 2, 3)], inputData[, -c(1, 2, 3)]) 
df2[, c(4, 5, 6)] <- df2[, c(4, 5, 6)] + matrix(rnorm( 3 * nrow(df2)), ncol = 3)
colnames(df2) <- letters[1:6]
rownames(df2) <- NULL
cghData2 <- as.ffdf(df2, pattern)




rm(list = ls())

library(snowfall)
sfInit(parallel = TRUE, cpus = 4, type = "MPI")
sfLibrary(ff)
sfLibrary(GLAD)
sfClusterSetupRNG(type = "SPRNG")
mydir <- "/home/ramon/caca"
sfExport("mydir")
setwd(mydir)
sfClusterEval(setwd(mydir))


source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R")


sfExport("getChromValue", "getCGHValue", "ffListOut", "internalGLAD")

## following doesn't work.
#sfClusterEval(source("/home/ramon/bzr-local-repos/adacgh2/R-packages/ADaCGH/R/ADaCGH-2.R"))

pSegmentGLAD("cghData.RData", "chromData.RData")
