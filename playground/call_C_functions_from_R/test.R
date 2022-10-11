library(Rcpp)
setwd("/home/alessandro/web-of-life-tutorial/playground/call_C_functions_from_R/") 
sourceCpp("./fibonacci.cpp")
sourceCpp("./doubler.cpp")

fibCpp(15)
vec <- as.vector(seq(1,10))
timesTwoCpp(vec)