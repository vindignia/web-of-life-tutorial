from_wol_graph_to_incidence_matrix <- function(base_url, nw_name, wol_graph, inc_mat){
  
  # get info about species 
  my_info <- read.csv(paste0(base_url,"get_species_info.php?network_name=",nw_name))
  
  isResource <- my_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE
  
  # Add the "type" attribute to the vertices of the graph 
  V(wol_graph)$type <- !(isResource) 
  
  #is_bipartite(wol_graph)
  
  # convert the igraph object into incidence matrix 
  inc_mat <- as_incidence_matrix(
    wol_graph,
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