library(devtools)
library(rjson)
library(dplyr)
library(formattable)
library(ggplot2)
library(permute)

setwd("/home/alessandro/web-of-life-tutorial/playground") 

# LOAD EXTERNAL PACKAGES
# PACKAGE FROM vindigna github 
remove.packages("weboflife")
devtools::install_github("bascompte-lab/weboflife", force=TRUE)
library(weboflife)

n <- 37 # number of rows
m <- 32 # number of columns 
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    #M[i,j] <- runif(1, min = 0, max = 10) 
    M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary sparse
  }
}


df1 <- NULL
MM <- as.matrix((M>0))  
class(MM) <- "numeric" 
sum(MM)
values_fraction <- sum(MM)/(nrow(M)*ncol(M))

for (i in seq(1,1500,50)){
  
  print(paste0("iter = ", i))
  M_res <- weboflife::null_model(M, i, model = "swap")

  #MM
  #sum(MM)
  #M_res 
  #sum(M_res) 
  
  overlap_mat <- (M_res == MM)
  class(overlap_mat) <- "numeric"  
  
  overlap_deg <- sum(overlap_mat)/(nrow(M)*ncol(M))
  df1 <- rbind(df1, data.frame(i, overlap_deg, values_fraction))
}

colnames(df1) <- c("iter","overlap", "v_fraction")
df %>% formattable()

ggplot() +
  ggtitle(paste0("swap model efficiency M with ", round(values_fraction,3), " non-zero el.")) +
  geom_point(data = df, aes(iter, overlap), color = "blue", shape=1, size=1.5) +
  geom_point(data = df1, aes(iter, overlap), color = "dark red", shape=1, size=1.5) +
  geom_line(data = df, aes(iter, 1. - 2*v_fraction), color = "black", size=0.6) +
  geom_line(data = df, aes(iter, 1. - v_fraction), color = "grey", size=0.6) +
  labs(x = "iter", y = "overlap") + ylim(0,1)

#OSS:  
# Given a matrix M with just a black bubble of size n_b, 
# the minimum achievable overlap is N - 2*n_b => 1 - 2*frac (normalized by N)
#
# For sparse enough matrices a number of iterations of the order of the matrix size N = nr*nc 
# already realize the maximal randomization which approaches the limit  1 - 2*frac
