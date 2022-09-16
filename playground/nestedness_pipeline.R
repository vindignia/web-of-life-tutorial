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


df <- NULL 
df <- data.frame(0, 
                 nestedness(NM),
                 compute_overlap(NM, NM), 
                 values_fraction)
colnames(df) <- c("iter","nestedness","overlap", "v_fraction")

for (i in seq(1,2000,50)){
  
  print(paste0("iter = ", i))
  
  NM_swap <- weboflife::null_model(NM, i, model = "swap")

  df <- rbind(df, data.frame(iter = i, 
                             nestedness = nestedness(NM_swap),
                             overlap = compute_overlap(NM, NM_swap), 
                             v_fraction = values_fraction)
  )
}

df %>% formattable()

overlap_leg <- data.frame(
  x = 250,
  y = 0.9,
  label = "overlap"
)

nestedness_leg <- data.frame(
  x = 120,
  y = 0.6,
  label = "nestedness"
)


ggplot() +
  ggtitle(paste0("perfectly nested M with ", round(values_fraction,3), " non-zero el. (swap null)")) +
  geom_point(data = df, aes(iter, overlap), color = "blue", shape=1, size=1.5) +
  geom_point(data = df, aes(iter, nestedness), color = "dark red", shape=1, size=1.5) +
  geom_line(data = df, aes(iter, 2*v_fraction-1), color = "black", size=0.6) +
  # geom_line(data = df, aes(iter, 1. - v_fraction), color = "grey", size=0.6) +
  geom_text(data=overlap_leg, aes( x=x, y=y, label=label),
            color="blue",
            size=4 , angle=0) + 
  geom_text(data=nestedness_leg, aes( x=x, y=y, label=label),
            color="dark red",
            size=4 , angle=0) + 
  labs(x = "iter", y = "overlap, nestedness") + ylim(0,1)

#VISUALIZATION:  
# NM <- perfect_nested(n,m)
# visualize and save your **perfectly nested matrix**  
# plot(nested_mat, key=NULL, border=NA,  col.ticks=NA, # cex.axis=NA,
#      col=c('light grey','blue'), breaks=c(0, 0.5, 1))  
#
# NM_swap <- weboflife::null_model(NM, model = "swap")
# plot(nested_mat_swap, key=NULL, border=NA,  col.ticks=NA, # cex.axis=NA,
#      col=c('light grey','blue'), breaks=c(0, 0.5, 1))  


