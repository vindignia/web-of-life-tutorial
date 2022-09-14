library(devtools)
library(rjson)
library(dplyr)
library(formattable)
library(igraph)
library(ggplot2)
library(permute)

setwd("/home/alessandro/web-of-life-tutorial/playground") 
source("helper.R") 

n <- 280 # number of rows
m <- 160 # number of columns 
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    #M[i,j] <- runif(1, min = 0, max = 10) 
    M[i,j] <- floor(runif(1, min = 0, max = 1.15)) # binary sparse
  }
}

M_res <- swap_model_ale(M, 5000)

#M 
sum(M)
#M_res 
sum(M_res) 

