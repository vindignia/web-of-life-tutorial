library(devtools)
#library(rjson)
library(dplyr)
library(formattable)
library(ggplot2)
library(latex2exp)
#library(permute)

#setwd("/home/alessandro/web-of-life-tutorial/playground")
setwd("/home/alessandro/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/null_models_convergence/")
# source("nullmodels_R/swap_model_F.R")
# source("nullmodels_R/trim_network.R")

# LOAD EXTERNAL PACKAGES
# PACKAGE FROM vindigna github
# remove.packages("rweboflife")
# detach(package:rweboflife,unload=TRUE)
# devtools::install_github("vindignia/rweboflife", force=TRUE)
#devtools::install_github("bascompte-lab/rweboflife", force=TRUE)
# library(rweboflife)

n <- 400 # number of rows 37
m <- 400 # number of columns 32
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    #M[i,j] <- runif(1, min = 0, max = 10)
    M[i,j] <- floor(runif(1, min = 0, max = 1.2))  # binary sparse
  }
}
M <-  as.matrix((M > 0))
class(M) <- "numeric"

# df <- as.data.frame(M)
# path_to_folder <- "/home/alessandro/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/matrices/"
# write.csv(df,file=paste0(path_to_folder,"matrix.csv"),row.names=FALSE)

# write.table(M,file="matrix.txt",row.names=FALSE) # drops the rownames
# rweboflife::nestedness(M)
#
# # now the code within null_swap.cpp is the same as in  nestedness.cpp
# # More > clean and install linked the cpp source to the R function
# # TODO: implement the right code within /src/null_swap.cpp
# rweboflife::swap_modelCpp(M, 33)
# M_res <- rweboflife::null_model(M, model = "swap")

#rweboflife::swap_modelCpp(M, iter_max = 4)


df <- NULL

M <- as.matrix((M>0))
class(M) <- "numeric"
sum(M)
v_fraction <- sum(M)/(nrow(M)*ncol(M))

MM <- M #  current
null_model <- "curveball" # "swap" # "cell" # "equifrequent" # "ale" #"fern"

for (i in 0:3000){

#  M_res <- weboflife::null_model(M, model = null_model)
  #M_res <- swap_model_F(MM,i)
  # M_res <- checkerboard_swap_model(MM, 1)

  # M_res <- rweboflife::null_model(MM, 1, model = "swap")
  # M_res <- null_model(MM, iter_max = 1, model = "swap")
  M_res <- null_model(MM, iter_max=1, model = null_model)

  overlap_mat <- (M_res == M)
  class(overlap_mat) <- "numeric"
  overlap_deg <- sum(overlap_mat)/(nrow(M)*ncol(M))

  MM <- M_res


  if(i%%20 == 0){
    print(paste0("iter = ", i))
    df <- rbind(df, data.frame(iter=i,
                               overlap=overlap_deg,
                               v_fraction=values_fraction,
                               model=null_model))
  }
}




colnames(df) <- c("iter","overlap", "v_fraction","model")

# remove a dataset
#df <- filter(df, fct!="cell")
#df %>% formattable()

swap_mod <- data.frame(
  x = 250,
  y = 0.95,
  label = "swap"
)
curveball_mod <- data.frame(
  x = 250,
  y = 0.65,
  label = "curveball"
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

#O_inf <- 0.72
#df_200 <-df
#l <-400
#df_300 <- df
#l <-600
#df_400 <- df
#l <-800
#df_500 <- df
#l <-1000

#save(df_200, df_500, file ="df_2023-08-21.RData")

# Empirically it seems to decay exponentially with a decay constant roughly 2*sqrt(nr*nc)

O_inf <- 0.723
ls <- 350*2.5
lc <- 200*2.4

ggplot() +
  ggtitle(paste0("null models convergence M with ", round(v_fraction,3), " non-zero el.")) +
  stat_function(fun = function(x) {(1-O_inf)*exp(-x/ls) + O_inf}, color ="dark grey", linetype = "solid", size = 0.8) +
  stat_function(fun = function(x) {(1-O_inf)*exp(-x/lc) + O_inf}, color ="dark grey", linetype = "solid", size = 0.8) +
# N = 200
  geom_point(data = filter(df_200, model=="swap"), aes(iter, overlap), color = "blue", shape=1, size=1.5) +
  geom_point(data = filter(df_200, model=="curveball"), aes(iter, overlap), color = "dark green", shape=0, size=1.5) +
# N = 500
  geom_point(data = filter(df_500, model=="swap"), aes(iter, overlap), color = "blue", shape=2, size=1.5) +
  geom_point(data = filter(df_500, model=="curveball"), aes(iter, overlap), color = "dark green", shape=5, size=1.5) +
#
  # geom_line(data = df, aes(iter, 1. - 2*v_fraction), color = "black", size=0.5) +
  # geom_line(data = df, aes(iter, 1. - v_fraction), color = "dark grey", size=0.5) +
  #labs(x = , y = "overlap") +
  xlab("iterations") +
  ylab(TeX("overlap $(M, M_{rnd})$")) +
  ylim(0.5,1)
  # geom_vline(xintercept = nrow(M)*ncol(M), linetype="dotted", color = "black", size=0.5) +
  # geom_vline(xintercept = nrow(M)*ncol(M)*1, linetype="dotted", color = "dark grey", size=0.5) +
  # geom_text(data=swap_mod, aes(x=x, y=y, label=label),
  #           color="blue",
  #           size=4 , angle=0) +
  # geom_text(data=curveball_mod, aes(x=x, y=y, label=label),
  #         color="dark green",
  #         size=4 , angle=0)
  # geom_text(data=cell_mod, aes(x=x, y=y, label=label),
  #           color="red",
  #           size=4 , angle=0) +
  # geom_text(data=mat_size, aes(x=x, y=y, label=label),
  #           color="black",
  #           size=4 , angle=0)



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



# check the performance of different algorithms
library(rbenchmark)

n <- 400
m <- 400
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    M[i,j] <- floor(runif(1, min = 0, max = 1.2))  # binary sparse
  }
}
M <-  as.matrix((M > 0))
class(M) <- "numeric"

rbenchmark::benchmark(
  "null_model_curveball" = {
    null_model(M, iter_max=ceiling(1.8*ncol(M)), model = 'curveball')
  },
  "null_model_swap" = {
    null_model(M, iter_max=ceiling(1.8*ncol(M)), model = 'swap')
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

