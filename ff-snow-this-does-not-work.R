
### changing values in ff object will not work in multinode-cluster (yes in SMP)
sfClusterEval(open(x1, readonly = FALSE))
sfClusterApplyLB(1:15, function(i) x1[, i] <- rep(nodenum, 4)) 
sfClusterEval(x1[, ])
sfClusterEval(close(x1))
sfClusterEval(filename(x1))

### Open it in master node and check what is inside
rm(x1)
load("x1.RData")
open(x1)
x1[, ] ## changes are stored (at least most of the time) in SMP





### Repeating the ff example, but by row.

##   Getting and setting values of ff object in slaves
x1 <- ff(1:18, dim = c(6, 3), filename = fullname1)

close(x1)
save(file = "x1.RData", x1)


## Acessing values; read-only
sfClusterEval(load("x1.RData"))
sfClusterEval(open(x1, readonly = TRUE))
sfClusterApplyLB(1:6, function(i) {list(nodenum = nodenum, values = x1[i, ])})
sfClusterEval(close(x1))


## What if I change things? This does not work in multinode-cluster (yes in SMP)
sfClusterEval(open(x1, readonly = FALSE))
sfClusterApplyLB(1:6, function(i) x1[i, ] <- rep(nodenum, 3)) 
sfClusterEval(x1[, ])
sfClusterEval(close(x1))
sfClusterEval(filename(x1))

### Open it in master node and check what is inside
rm(x1)
load("x1.RData")
open(x1)
x1[, ] ## changes are stored (at least most of the time) in SMP

