
## RcppMatrixExample.R: RcppMatrix example
##
## Copyright (C) 2008         Dirk Eddelbuettel
## Copyright (C) 2009 - 2016  Dirk Eddelbuettel and Romain Francois
library(Rcpp)
setwd("/home/alessandro/web-of-life-tutorial/playground/call_C_functions_from_R/RcppExamples") 
sourceCpp("./src/MatrixExample.cpp")

my_mat <- matrix(seq(1,12)^2, ncol=4)
my_mat
val <- MatrixExample(my_mat)
val
