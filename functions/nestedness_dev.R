library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(bipartite) 

base_url <- "https://www.web-of-life.es/" 

nw_name <- "M_PL_073" # "FW_017_02" # "M_PL_052" #Klementyna


# info
my_info <- read.csv(paste0(base_url,"get_species_info.php?network_name=",nw_name))

isResource <- my_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE

# Add the "type" attribute to the vertices of the graph 
V(my_graph)$type <- !(isResource) 

#  assign distinct colors to nodes of the two guilds 
V(my_graph)$color <- ifelse(V(my_graph)$type == TRUE, "lightblue", "lightgreen")
# remove arrows from links 
E(my_graph)$arrow.mode = "-"

plot(my_graph, 
     layout=layout_as_bipartite, # layout_nicely,  
     arrow.mode=0,
     vertex.label=NA,
     vertex.size=4,
     asp=0.2)

# Nestedness 

# source("./functions/compute_nestedness.R")
# B_ex <- read.csv(file = "../ecological_networks_2022/downloads/Data/03-23_measuring_nestedness/B_example.csv")
# B <- as.matrix(B_ex)
# dim(B)
# dim(B_ex) 
# compute_nestedness(B) # not giving the result of Klementyna 


# convert the igraph object into incidence matrix 
inc_matrix <- as_incidence_matrix(
  my_graph,
  attr = "connection_strength",
  names = TRUE,
  sparse = FALSE
)

# convert elements into numeric values 
class(inc_matrix) <-"numeric"

# remove NA values (previous empty strings)
inc_matrix[which(is.na(inc_matrix) == T)]<-0
# check dimensions and compare with the number of species on the Web interface 
dim(inc_matrix)

source("./functions/compute_nestedness.R")
compute_nestedness(inc_matrix)

networklevel(inc_matrix, 
             index=c("connectance", "weighted nestedness","cluster coefficient"), 
             SAmethod="log")


links <- my_nw %>% nrow() 
plants <- distinct(my_nw, species2) %>%  nrow() 
animals <- distinct(my_nw, species1) %>% nrow()
size <- plants*animals
conn <- links/size    
c_bipartite <- links/(plants+animals)

# FOODWEB test 
# download the foodweb FW_002
fw_name <- "FW_017_02"
json_url <- paste0(base_url,"get_networks.php?network_name=",fw_name)
fw <- jsonlite::fromJSON(json_url)

# select the 3 relevant column and pass and create the igraph object 
fw_graph <- fw %>% select(species1, species2, connection_strength) %>% 
  graph_from_data_frame(directed = TRUE)

# convert the igraph object into adjacency matrix 
adj_matrix <- as_adjacency_matrix(fw_graph, 
                                  type = "lower", # it is directed!!!
                                  attr = "connection_strength",
                                  sparse = FALSE)  
# convert elements into numeric values 
class(adj_matrix) <-"numeric"
# remove NA values (previous empty strings)
adj_matrix[which(is.na(adj_matrix) == T)]<-0

dim(adj_matrix)
# def of the paper 
# https://arxiv.org/pdf/0808.3397.pdf

links <- fw %>% nrow() 
size <- nrow(adj_matrix)
conn <- 2*links/(size*(size-1))     # this one matches the DB calculation     
conn_bipartite <- links/(size*size)   # no idea how bipartite computes the connectance          

networklevel(adj_matrix, 
             index=c("connectance", "weighted nestedness","cluster coefficient"), 
             SAmethod="log")
