power_decay <- function(x,c,alpha){
  return(c/x**alpha)
}

incidence_matrix_from_dataframe <- function(data){
  #############################################
  # Initialize empty matrix to fill
  #############################################
  unique_partner_A = unique(data[,1]) # find unique partner A
  unique_partner_B = unique(data[,2]) # find unique partner B
  incidence_matrix = matrix(0L,length(unique_partner_A),length(unique_partner_B)) # create empty matrix
  rownames(incidence_matrix) = as.character(unique_partner_A) # Set names of row species, according to unique partner A
  colnames(incidence_matrix) = as.character(unique_partner_B) # Set names of row species, according to unique partner B
  
  colnames(data) <- c("speciesName1", "speciesName2", "connectionStrength")
  
  #############################################
  # Fill matrix based on data
  #############################################
  
  for (species in rownames(incidence_matrix)) {
    for (partner in colnames(incidence_matrix)) {
      tmp <- data %>% filter(speciesName1 == species & speciesName2 == partner)
      if(nrow(tmp) == 0) {
        interaction_strength <- 0
      } else {
        interaction_strength <- tmp %>% select(connectionStrength) %>% unlist() %>% as.numeric() 
      }
      incidence_matrix[as.character(species),as.character(partner)] <- interaction_strength
    }
  }
  
  return(incidence_matrix)
}


convert_data_to_edge_list <- function(data){
  
  plant_names <- distinct(data, plant)
  animal_names <- distinct(data, insect)
  
  edge_list <- NULL         
  
  
  for (species1 in plant_names$plant) {
    for (species2 in animal_names$insect) {
      
      conn_strength <- data %>% filter(plant==species1) %>% filter(insect==species2) %>% nrow()
      
      if(conn_strength>0){
        my_row <- data.frame(species1, species2, conn_strength)
        edge_list <- rbind(edge_list, my_row)  
      }
    }
  }
  
  colnames(edge_list) <- c("species1", "species2", "connection_strength")
  

  return(edge_list)
  
}

perfect_nested <- function(my_rows, my_cols){
  
  mat <- matrix(0, my_rows, my_cols)
  
  for (i in seq(1,my_rows,1)) {
    j_max <- ceiling(i*my_cols/my_rows)
    
    # print(i)
    # print(j_max)    
    # print("")
    
    for (j in seq(1,j_max,1)) {
      mat[i,j] <-1
    }
  }
  return(mat)
} 



nested_power_law <- function(my_rows, my_cols, alpha){
  
  mat <- matrix(1, my_rows, my_cols)
  
  my_cols_alpha <- my_cols**alpha
  B_alpha <- as.double(my_rows -1)*my_cols_alpha/(my_cols_alpha-1)
  B <- B_alpha**(1./alpha) 
  A <- B_alpha + 1 
  
  for (j in seq(1,my_cols,1)) {
    tmp <- (B/j)**alpha
    i_max <- ceiling(A - tmp)
    if(i_max > my_rows){
      i_max <- my_rows
    }
      
    # print(j)
    # print(i_max)    
    # print("")
    
    for (i in seq(1,i_max,1)) {
      mat[i,j] <- 0
    }
  }
  return(mat)
} 


species_df_from_matrix <- function(matrix){
  
  # assign row names 
  species <- rep(c("row sp "),times=nrow(matrix),1)
  my_rows <- seq(1,nrow(matrix),1) %>% 
    as.array() %>%
    as.character()
  my_rows <- paste0(species, my_rows)
  
  rownames(matrix) <- my_rows 
  
  # assign column names 
  species <- rep(c("col sp "),times=ncol(matrix),1)
  my_cols <- seq(1,ncol(matrix),1) %>% 
    as.array() %>%
    as.character()
  my_cols <- paste0(species, my_cols)
  
  colnames(matrix) <- my_cols 
  
  return(as.data.frame(matrix))
  
}

connectance_from_edgelist <- function(nw){
  links <- nw %>% nrow() 
  plants <- distinct(nw, species2) %>%  nrow() 
  animals <- distinct(nw, species1) %>% nrow()
  size <- plants*animals
  conn <- links/size    
  return(conn)
}


diluted_matrix <- function(mat, connectance)
{
  
  print(paste0("nominal connectance = ", connectance))
  
  sum_max <- nrow(mat)*ncol(mat)*connectance
  diluted_mat <- mat
  repeat{
    irnd <- sample(1:nrow(mat),5)
    jrnd <- sample(1:ncol(mat),5)
    # TO optimize 
    for (i in irnd){
      for (j in jrnd){
        #print(paste("assign zero to ", i,j))
        diluted_mat[i,j] <- 0
      }
    }
    
    
    if(sum(diluted_mat) < sum_max)  {
      print("exit") 
      break
    }
  }
  
  return(diluted_mat)
}  

incidence_matrix_from_graph <- function(my_graph){

  # Add the "type" attribute to the vertices of the graph 
  V(my_graph)$type <- bipartite_mapping(my_graph)$type  
  
  my_inc_mat <- as_incidence_matrix(
    my_graph,
    attr = "connection_strength",
    names = TRUE,
    sparse = FALSE
  )
  
  # convert elements into numeric values 
  class(my_inc_mat) <-"numeric"
  
  # remove NA values (previous empty strings)
  my_inc_mat[which(is.na(my_inc_mat) == T)]<-0
  
  return(my_inc_mat)
}


edge_list_bin_from_matrix_df <- function(df) 
{
  edge_list_bin <-NULL
  for (species2 in colnames(df)){
    for (species1 in rownames(df)){
      if(!(df[species1,species2]==0)){
        my_row <- data.frame(species1, species2, as.integer(1))
        edge_list_bin <- rbind(edge_list_bin, my_row)
      }
    }
  }
  
  colnames(edge_list_bin) <- c("species1", "species2", "connection_bin")
  
  return(edge_list_bin)
}


compute_overlap <- function(M1, M2){
  overlap_mat <- (M1 == M2)
  class(overlap_mat) <- "numeric"  
  overlap_deg <- sum(overlap_mat)/(nrow(M1)*ncol(M1))
  return(overlap_deg)
}
