library(devtools)
library(rjson)
library(dplyr)
library(formattable)
library(ggplot2)
library(permute)
#library(plot.matrix)

setwd("/home/alessandro/web-of-life-tutorial/playground") 
# source("../functions/compute_nestedness.R")
source("../functions/helper.R")


# LOAD EXTERNAL PACKAGES
# PACKAGE FROM bascompte-lab github 
remove.packages("weboflife")
devtools::install_github("bascompte-lab/weboflife", force=TRUE)
library(weboflife)

n <- 40 # number of rows
m <- 30 # number of columns 
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary sparse
  }
}

weboflife::nestedness(M)

# Perfectly nested 
NM <- perfect_nested(n,m)
NM <- as.matrix((NM>0))  
class(NM) <- "numeric" 
sum(NM)
values_fraction <- sum(NM)/(nrow(NM)*ncol(NM))

nestedness_in <- weboflife::nestedness(NM)
overlap_in <- compute_overlap(NM, NM)

df <- NULL 
df <- data.frame(0,nestedness_in,nestedness_in,nestedness_in,
                 overlap_in,overlap_in,overlap_in,values_fraction)

colnames(df) <- c("iter","nest_equif","nest_cell","nest_swap",
                  "overlap_equif","overlap_cell","overlap_swap",
                  "v_fraction")


for (i in seq(1,2001,50)){
  
  print(paste0("iter = ", i))
  
  NM_equif <- weboflife::null_model(NM, i, model = "equifrequent")
  NM_cell <- weboflife::null_model(NM, model = "cell")
  NM_swap <- weboflife::null_model(NM, i, model = "swap")
  
  df <- rbind(df, data.frame(iter = i, 
                             nest_equif = weboflife::nestedness(NM_equif),
                             nest_cell= weboflife::nestedness(NM_cell),
                             nest_swap = weboflife::nestedness(NM_swap),
                             overlap_equif = compute_overlap(NM, NM_equif), 
                             overlap_cell = compute_overlap(NM, NM_cell), 
                             overlap_swap = compute_overlap(NM, NM_swap), 
                             v_fraction = values_fraction)
  )
}

df %>% formattable()

overlap_leg <- data.frame(
  x = 750,
  y = 0.9,
  label = "overlap"
)

nestedness_leg <- data.frame(
  x = 800,
  y = 0.8,
  label = "nestedness"
)


ggplot() +
  ggtitle(paste0("perfectly nested M with ", round(values_fraction,3), " non-zero el. (swap null)")) +
  geom_point(data = df, aes(iter, overlap_equif), color = "blue", shape=2, size=1.5) +
  geom_point(data = df, aes(iter, nest_equif), color = "dark red", shape=2, size=1.5) +
  #
  geom_point(data = df, aes(iter, overlap_cell), color = "blue", shape=3, size=1.5) +
  geom_point(data = df, aes(iter, nest_cell), color = "dark red", shape=3, size=1.5) +
  #
  geom_point(data = df, aes(iter, overlap_swap), color = "blue", shape=1, size=1.5) +
  geom_point(data = df, aes(iter, nest_swap), color = "dark red", shape=1, size=1.5) +
  #
  geom_line(data = df, aes(iter, 2*v_fraction-1), color = "black", size=0.6) +
  # geom_line(data = df, aes(iter, 1. - v_fraction), color = "grey", size=0.6) +
  geom_text(data=overlap_leg, aes( x=x, y=y, label=label),
            color="blue",
            size=4 , angle=0) + 
  geom_text(data=nestedness_leg, aes( x=x, y=y, label=label),
            color="dark red",
            size=4 , angle=0) + 
  labs(x = "iter", y = "overlap, nestedness") + ylim(0,1)


# NULL models applied to nestedness

nest_df <- NULL 

for (ii in seq(1,100)){
  
  print(paste0("iter = ", ii))
  
  NM_equif <- weboflife::null_model(NM, model = "equifrequent")
  NM_cell <- weboflife::null_model(NM, model = "cell")
  NM_swap <- weboflife::null_model(NM, model = "swap")
  
  nest_df <- rbind(nest_df, data.frame(iter = ii, 
                                       nest_equif = weboflife::nestedness(NM_equif),
                                       nest_cell= weboflife::nestedness(NM_cell),
                                       nest_swap = weboflife::nestedness(NM_swap)
                                       ))
}

colnames(nest_df) <- c("iter","nest_equif","nest_cell","nest_swap")
nest_df %>% formattable()

# # Compute z-score for the nestedness values estimated by different null models 

z_equif <- (nestedness_in - mean(nest_df$nest_equif))/sd(nest_df$nest_equif)
z_cell <- (nestedness_in - mean(nest_df$nest_cell))/sd(nest_df$nest_cell)
z_swap <- (nestedness_in - mean(nest_df$nest_swap))/sd(nest_df$nest_swap)


#Fernando 

#pollination_networks
load("nullmodels_R/03-24_null_models_data/pollination_networks.Rdata")
#seed_networks
load("nullmodels_R/03-24_null_models_data/seed_networks.Rdata")

# TODO: implement download of the same networks and convert in a suitable input for null models  
# TODO: implement the loop above into a function which takes 
#       the matrix, number of samplings and returns the z factor.   


class(pollination_networks)
pollination_networks$M_PL_008


#VISUALIZATION:  
# NM <- perfect_nested(n,m)
# visualize and save your **perfectly nested matrix**  
# plot(nested_mat, key=NULL, border=NA,  col.ticks=NA, # cex.axis=NA,
#      col=c('light grey','blue'), breaks=c(0, 0.5, 1))  
#
# NM_swap <- weboflife::null_model(NM, model = "swap")
# plot(nested_mat_swap, key=NULL, border=NA,  col.ticks=NA, # cex.axis=NA,
#      col=c('light grey','blue'), breaks=c(0, 0.5, 1))  


