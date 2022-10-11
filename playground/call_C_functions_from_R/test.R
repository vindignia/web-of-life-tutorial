library(Rcpp)
setwd("/home/alessandro/web-of-life-tutorial/playground/call_C_functions_from_R/") 
sourceCpp("./helper.cpp")


fibCpp(6)
factCpp(6)

vec <- as.vector(seq(1,10))
timesTwoCpp(vec)