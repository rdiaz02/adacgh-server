fP <- function(x, y, ...) { x * y}
fA <- function(x, y, ...) { x + y }
fAA <- function(x, y , v, ...) { x + y + v}

my.internal <- function(x, method, ...) {
  method.string <- deparse(substitute(method))
  cat("the method string is ", method.string, "\n")
  method(x, ...)
}


sfClusterApplyLB(1:10, my.internal, fA, 3)
sfClusterApplyLB(1:10, my.internal, fP, 3)
sfClusterApplyLB(1:10, my.internal, fAA, 3, 13)



w1 <- function(indices, method, ...) {
  sfClusterApplyLB(indices, method, ...)
}

w1(1:10, fP, 3)
w1(1:4, fA, 7)
w1(1:4, fAA, 3, 10)
w1(1:4, fAA, 3) ## fails, of course




xv <- 1:95
save(file = "xv.RData", xv)

f5 <- function() {
browser()
browser()
}
