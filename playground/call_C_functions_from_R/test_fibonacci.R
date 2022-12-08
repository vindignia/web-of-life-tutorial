# benchmark C against R 
library(rbenchmark)
library(Rcpp)
setwd("/home/alessandro/web-of-life-tutorial/playground/call_C_functions_from_R/") 
sourceCpp("./fibonacci.cpp")

## create the R function to compute the sum of Fibonacci number till n 
fibR <- function(n) {
  if ((n == 0) | (n == 1)) 
    return(1)
  else
    return(fibR(n-1) + fibR(n-2))
}

m = 25 # single run  

fibR(m) 
fibCpp(m)

# benchmark 
benchmark(fibR(m), fibCpp(m), replications = 30)[,1:4]

