library(devtools)
#library(rjson)
library(dplyr)
library(formattable)
library(ggplot2)
library(latex2exp)
#library(permute)

setwd("/home/alessandro/web-of-life-tutorial/playground") 
# source("nullmodels_R/swap_model_F.R")
# source("nullmodels_R/trim_network.R")

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
    M[i,j] <- floor(runif(1, min = 0, max = 3.2)) # binary sparse
  }
}


df <- NULL

MM <- as.matrix((M>0))  
class(MM) <- "numeric" 
sum(MM)
values_fraction <- sum(MM)/(nrow(M)*ncol(M))

null_model <- "swap" # "cell" # "equifrequent" # "ale" #"fern" 

for (i in seq(1,100,1)){ # seq(1,1501,50)){
  
  print(paste0("iter = ", i))
  
  M_res <- weboflife::null_model(M, model = null_model)
  #M_res <- swap_model_F(MM,i)  

  overlap_mat <- (M_res == MM)
  class(overlap_mat) <- "numeric"  
  
  overlap_deg <- sum(overlap_mat)/(nrow(M)*ncol(M))
  df <- rbind(df, data.frame(iter=i, 
                             overlap=overlap_deg, 
                             v_fraction=values_fraction, 
                             model=null_model))
}

colnames(df) <- c("iter","overlap", "v_fraction","model")

# remove a dataset 
#df <- filter(df, fct!="cell") 
df %>% formattable()

swap_mod <- data.frame(
  x = 150,
  y = 0.95,
  label = "swap"
)
cell_mod <- data.frame(
  x = 150,
  y = 0.65,
  label = "cell"
)
equif_mod <- data.frame(
  x = 150,
  y = 0.52,
  label = "equifrequent"
)
mat_size <- data.frame(
  x = nrow(M)*ncol(M) - 150,
  y = 0.2,
  label = "matrix size"
)


ggplot() +
  ggtitle(paste0("null models convergence M with ", round(values_fraction,3), " non-zero el.")) +
  geom_point(data = filter(df, model=="swap"), aes(iter, overlap), color = "blue", shape=1, size=1.5) +
  geom_point(data = filter(df, model=="equifrequent"), aes(iter, overlap), color = "dark green", shape=1, size=1.5) +
  geom_point(data = filter(df, model=="cell"), aes(iter, overlap), color = "red", shape=1, size=1.5) +
  geom_line(data = df, aes(iter, 1. - 2*v_fraction), color = "black", size=0.5) +
  geom_line(data = df, aes(iter, 1. - v_fraction), color = "dark grey", size=0.5) +
  #labs(x = , y = "overlap") +
  xlab("iterations") + 
  ylab(TeX("overlap $(M, M_{rnd})$")) + 
  ylim(0,1) +
  geom_vline(xintercept = nrow(M)*ncol(M), linetype="dotted", color = "black", size=0.5) + 
  geom_vline(xintercept = nrow(M)*ncol(M)*2/3, linetype="dotted", color = "dark grey", size=0.5) + 
  geom_text(data=swap_mod, aes(x=x, y=y, label=label),
            color="blue",
            size=4 , angle=0) + 
  geom_text(data=equif_mod, aes(x=x, y=y, label=label),
          color="dark green",
          size=4 , angle=0) + 
  geom_text(data=cell_mod, aes(x=x, y=y, label=label),
            color="red",
            size=4 , angle=0) + 
  geom_text(data=mat_size, aes(x=x, y=y, label=label),
            color="black",
            size=4 , angle=0)



#OSS:  
# Given a matrix M with just a black bubble of size n_b, 
# the minimum achievable overlap is N - 2*n_b => 1 - 2*frac (normalized by N)
#
# For sparse enough matrices a number of iterations of the order of the matrix size N = nr*nc 
# already realize the maximal randomization which approaches the limit  1 - 2*frac
#
# swap_model developed by Fernando gives the same convergence curve as the one developed by Alessandro  
#
# from the cell and equifrequent swap models i removed the trim() call
# otherwise a matrix of different size is returned and one cannot define the overlap 