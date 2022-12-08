library(devtools)
library(rjson)
# library(dplyr)
# library(formattable)
library(rbenchmark)
library("JuliaCall")
julia <- julia_setup()
library(Rcpp)


setwd("/home/alessandro/web-of-life-tutorial/playground")
julia_source("nestedness.jl")
sourceCpp("nestedness.cpp")


# LOAD EXTERNAL PACKAGES
# PACKAGE FROM bascompte-lab github
remove.packages("weboflife")
devtools::install_github("bascompte-lab/weboflife", force = TRUE)
#devtools::install_github("bascompte-lab/weboflife@rcpp-nestedness", force = TRUE)
library(weboflife)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)


n <- 252 # number of rows
m <- 285 # number of columns
M <- matrix(0, n, m)

for (i in 1:n) {
  for (j in 1:m) {
    #M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary sparse
    M[i, j] <- runif(1, min = 0, max = 1.2)
    M[i, j] <- if (M[i, j] > 0.6)
      M[i, j]
    else
      0.
  }
}
print("matrix dimensions")
dim(M)

# compare the Julia vs C++

rbenchmark::benchmark(
  "nestedness_julia" = {
    julia_call("nestedness", M)
  },
  "nestedness_cpp" = {
    nestednessCpp(M)
  },
  "nestedness_cpp wol" = {
    weboflife::nestednessCpp(M)
  },
  replications = 10,
  columns = c(
    "test",
    "relative",
    "replications",
    "elapsed",
    "user.self",
    "sys.self"
  )
)
