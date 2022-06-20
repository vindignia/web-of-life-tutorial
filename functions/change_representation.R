from_web_of_life_to_incidence_matrix <- function(base_url, nw_name, inc_mat){
  
  # download nw data 
  json_url <- paste0(base_url,"get_networks.php?network_name=",nw_name)
  my_nw <- jsonlite::fromJSON(json_url)
  
  # select the 3 relevant columns and create the igraph object 
  my_graph <- my_nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = TRUE)
  
  # info
  my_info <- read.csv(paste0(base_url,"get_species_info.php?network_name=",nw_name))
  
  isResource <- my_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE
  
  # Add the "type" attribute to the vertices of the graph 
  V(my_graph)$type <- !(isResource) 
  
  #is_bipartite(my_graph)
  
  # convert the igraph object into incidence matrix 
  inc_mat <- as_incidence_matrix(
    my_graph,
    attr = "connection_strength",
    names = TRUE,
    sparse = FALSE
  )
  
  # convert elements into numeric values 
  class(inc_mat) <-"numeric"
  
  # remove NA values (previous empty strings)
  inc_mat[which(is.na(inc_mat) == T)]<-0
  
  return(inc_mat) 
}