library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(bipartite) 


library(devtools)
#install_github("yixuan/RSpectra")

#library(RSpectra)


base_url <- "https://www.web-of-life.es/" 

#nw_name <- "M_PL_073" # "A_PH_003" # "M_PA_002" # " # "FW_017_02" # "M_PL_052" #Klementyna

# downlaod foodwebs 

json_url <- paste0(base_url,"get_networks.php?interaction_type=FoodWebs") 
fw_nws <- jsonlite::fromJSON(json_url)
nw_names <- distinct(fw_nws,network_name) 

FW_001_graph <- FW_001_nw %>% select(species1, species2, connection_strength) %>% 
  graph_from_data_frame(directed = TRUE)

# info
my_info <- read.csv(paste0(base_url,"get_species_info.php?network_name=",nw_name))
isResource <- my_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE


# download my network
json_url <- paste0(base_url,"get_networks.php?network_name=",nw_name)
nw <- jsonlite::fromJSON(json_url)

links <- nw %>% nrow() 

my_graph <- nw %>% select(species1, species2, connection_strength) %>% 
  graph_from_data_frame(directed = FALSE)


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

# Modularity

modularity_df <- NULL         

iter_range <- 1:1

for (iter in iter_range) {
  
  print(paste0("iteration ", iter)) 

  modules1 <- cluster_fast_greedy(my_graph)
  modules2 <- cluster_edge_betweenness(my_graph)
  modules3 <- cluster_leading_eigen(my_graph)
  #modules4 <- cluster_optimal(my_graph) 
  
  mod_row <- data.frame(modularity(modules1, resolution=1), 
                        modularity(modules2, resolution=1), 
                        modularity(modules3, resolution=1))
          
  modularity_df <- rbind(modularity_df,mod_row)
}

colnames(modularity_df) <- c("modularity_fast_greedy",
                       "modularity_edge_betweenness",
                       "modularity_leading_eigen")

modularity_df %>% formattable()

modules <- modules3
colbar <- rainbow(max(modules$membership))
V(my_graph)$color <- colbar[modules$membership]

plot(my_graph, 
     layout=layout_with_fr, 
     vertex.size=12, 
     vertex.label=NA,
     margin=0.0)



