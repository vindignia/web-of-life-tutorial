library(devtools)
#library(rjson)
library(dplyr)
library(formattable)
library(ggplot2)
library(latex2exp)
#library(permute)

#setwd("/home/alessandro/web-of-life-tutorial/playground")
setwd("/home/alessandro/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground")
# source("nullmodels_R/swap_model_F.R")
# source("nullmodels_R/trim_network.R")

# LOAD EXTERNAL PACKAGES
# PACKAGE FROM vindigna github
remove.packages("rweboflife")
detach(package:rweboflife,unload=TRUE)
devtools::install_github("vindignia/rweboflife", force=TRUE)
#devtools::install_github("bascompte-lab/rweboflife", force=TRUE)
library(rweboflife)

n <- 128 #200 #137 # number of rows 37
m <- 93 # 200 # number of columns 32
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    #M[i,j] <- runif(1, min = 0, max = 10)
    M[i,j] <- floor(runif(1, min = 0, max = 1.2))  # binary sparse
  }
}
M <-  as.matrix((M > 0))
class(M) <- "numeric"
df <- as.data.frame(M)
path_to_folder <- "/home/alessandro/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/matrices/"
write.csv(df,file=paste0(path_to_folder,"matrix.csv"),row.names=FALSE)

# write.table(M,file="matrix.txt",row.names=FALSE) # drops the rownames
# rweboflife::nestedness(M)
#
# # now the code within null_swap.cpp is the same as in  nestedness.cpp
# # More > clean and install linked the cpp source to the R function
# # TODO: implement the right code within /src/null_swap.cpp
# rweboflife::swap_modelCpp(M, 33)
# M_res <- rweboflife::null_model(M, model = "swap")

rweboflife::swap_modelCpp(M, iter_max = 4)

all(rows_tot_rnd == rows_tot_original)


print("check col total")
cols_tot_rnd <- c()
cols_tot_original <- c()

print("check col sums")
for(j in seq(1,ncol(M))){
  cols_tot_rnd <- append(cols_tot_rnd, sum(M_res[,j]))
  cols_tot_original <- append(cols_tot_original, sum(M_res[,j]))
  print(c(sum(M[,j]),  sum(M_res[,j])))
}

all(cols_tot_rnd == cols_tot_original)


df <- NULL

M <- as.matrix((M>0))
class(M) <- "numeric"
sum(M)
values_fraction <- sum(M)/(nrow(M)*ncol(M))

MM <- M #  current
null_model <- "swap" # "cell" # "equifrequent" # "ale" #"fern"

for (i in 0:5000){



#  M_res <- weboflife::null_model(M, model = null_model)
  #M_res <- swap_model_F(MM,i)
  # M_res <- checkerboard_swap_model(MM, 1)

  M_res <- rweboflife::null_model(MM, 1, model = "swap")

  overlap_mat <- (M_res == M)
  class(overlap_mat) <- "numeric"
  overlap_deg <- sum(overlap_mat)/(nrow(M)*ncol(M))

  MM <- M_res


  if(i%%50 == 0){
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

O_inf <- 0.72
#df_200 <-df
#l <-400
#df_300 <- df
#l <-600
#df_400 <- df
#l <-800
df_500 <- df
l <-1000

#save(df_200, df_300, df_400, df_500, file ="convergence_df.RData")

# Empirically it seems to decay exponentially with a decay constant roughly 2*sqrt(nr*nc)

ggplot() +
  ggtitle(paste0("null models convergence M with ", round(values_fraction,3), " non-zero el.")) +
  geom_point(data = filter(df, model=="swap"), aes(iter, overlap), color = "blue", shape=1, size=1.5) +
#  geom_point(data = filter(df, model=="equifrequent"), aes(iter, overlap), color = "dark green", shape=1, size=1.5) +
#  geom_point(data = filter(df, model=="cell"), aes(iter, overlap), color = "red", shape=1, size=1.5) +
  stat_function(fun = function(x) {(1-O_inf)*exp(-x/l) + O_inf}, color ="grey", linetype = "dotted", size = 1.5) +
  geom_line(data = df, aes(iter, 1. - 2*v_fraction), color = "black", size=0.5) +
  geom_line(data = df, aes(iter, 1. - v_fraction), color = "dark grey", size=0.5) +
  #labs(x = , y = "overlap") +
  xlab("iterations") +
  ylab(TeX("overlap $(M, M_{rnd})$")) +
  ylim(0,1)
  # geom_vline(xintercept = nrow(M)*ncol(M), linetype="dotted", color = "black", size=0.5) +
  # geom_vline(xintercept = nrow(M)*ncol(M)*1, linetype="dotted", color = "dark grey", size=0.5) +
  # geom_text(data=swap_mod, aes(x=x, y=y, label=label),
  #           color="blue",
  #           size=4 , angle=0) +
  # geom_text(data=equif_mod, aes(x=x, y=y, label=label),
  #         color="dark green",
  #         size=4 , angle=0) +
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


#swap_model <- function(Mat,iter_max)

checkerboard_swap_model <- function(binary_matrix, iter_max = NULL) {
  rows <- nrow(binary_matrix)
  cols <- ncol(binary_matrix)

  if(is.null(iter_max)){
    iter_max <- ceiling(10*sqrt(rows*cols))
    warning(paste0("as the parameter iter_max was not assigned, the default value iter_max = ", iter_max," was assumed"))
  }

  ID <- matrix(c(1, 0, 0, 1), nrow = 2,byrow = TRUE)
  SX <- matrix(c(0, 1, 1, 0), nrow = 2,byrow = TRUE)

  # checkerboard_id_subsets <- list()
  # checkerboard_sx_subsets <- list()

  # Create a copy of the original matrix
  shuffled_matrix <- binary_matrix

  for (iter in 1:iter_max) {
    # Randomly select two distinct rows
    row_indices <- sample(rows, size = 2, replace = FALSE)
    row1 <- row_indices[1]
    row2 <- row_indices[2]
    # Randomly select two distinct columns
    col_indices <- sample(cols, size = 2, replace = FALSE)
    col1 <- col_indices[1]
    col2 <- col_indices[2]

    # subset a 2x2 matrix
    subset <- shuffled_matrix[c(row1,row2), c(col1,col2)]

    # If the subset has exactly the identity (checkerboard pattern)
    if (all(subset == ID)) {
      #checkerboard_id_subsets[[length(checkerboard_id_subsets) + 1]] <- matrix(c(row1,col1,row2,col2), nrow = 2, byrow = TRUE) # subset #
      # then swap
      shuffled_matrix[c(row1,row2), c(col1,col2)] <- SX
    } else if (all(subset == SX)) {
      #checkerboard_sx_subsets[[length(checkerboard_sx_subsets) + 1]] <- matrix(c(row1,col1,row2,col2), nrow = 2, byrow = TRUE) # subset #
      # then swap
      shuffled_matrix[c(row1,row2), c(col1,col2)] <- ID
    }
  }
  return(shuffled_matrix)
}

# Example usage
#BM <- matrix(c(1, 0, 0, 1, 0, 1, 1, 0), nrow = 4, byrow = TRUE)

MM <- checkerboard_swap_model(M,100)

# Print the detected subsets
for (subset in detected_subsets) {
  print(subset)
}

Hi, given the matrices A and B, how should i translate te R command
all(A == B)
that check if all elements of the A and B matrices are equal into Rcpp?