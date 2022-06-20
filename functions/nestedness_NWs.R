library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(bipartite) 

base_url <- "https://www.web-of-life.es/" 
source("./functions/compute_nestedness.R")
source("./functions/change_representation.R")

nw_name <- "M_PL_073" # "FW_017_02" # "M_PL_073" #  "M_PL_052" #Klementyna
 

inc_matrix <- from_web_of_life_to_incidence_matrix(base_url, nw_name, inc_mat)

compute_nestedness(inc_matrix)

networklevel(inc_matrix, 
             index=c("connectance", "weighted nestedness","cluster coefficient"), 
             SAmethod="log")
