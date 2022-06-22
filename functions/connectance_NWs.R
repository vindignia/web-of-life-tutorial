library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(bipartite) 

base_url <- "https://www.web-of-life.es/" 
source("./functions/change_representation.R")

# download all the networks
json_url <- paste0(base_url,"get_networks.php") 
all_nws <- jsonlite::fromJSON(json_url)
head(all_nws)

nw_names <- distinct(all_nws, network_name) %>% 
  dplyr::filter(., !(network_name %like% "FW_")) # filter out food-webs

# initialize dataframe to store results 
connectance_df <- NULL         

for (nw_name in nw_names$network_name){
  
  nw <- filter(all_nws, network_name == nw_name) 
  
  links <- nw %>% nrow() 
  
  # select the 3 relevant columns and create the igraph object 
  my_graph <- nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = FALSE)
  
  inc_matrix <- from_wol_graph_to_incidence_matrix(base_url, nw_name, my_graph, inc_matrix)
  
  
  nw_prop_bipartite <- networklevel(inc_matrix, 
                                    index=c("connectance"), 
                                    SAmethod="log")
  
  
  resources_num <- nrow(inc_matrix) 
  consumers_num <- ncol(inc_matrix)
  
  conn <- links/(resources_num*consumers_num)
  
  connectance_row <- data.frame(nw_name, 
                               resources_num, 
                               consumers_num, 
                               conn,
                               nw_prop_bipartite["connectance"])
  
  connectance_df <- rbind(connectance_df, connectance_row)
  
  print(paste0("connectance of ", nw_name, " network"))
  
}

rownames(connectance_df) <- NULL 
colnames(connectance_df) <- NULL
colnames(connectance_df) <- c("network_name", 
                             "num_resources", 
                             "num_consumers",
                             "connectance",
                             "bipartite_connectance")


connectance_df %>% formattable()

######### SAVE data #########
path = "~/web-of-life-tutorial/data/"
file_name = "connectance"

# save RData objects  
save(connectance_df, file = paste0(path, file_name,".RData"))

# write file to csv
write.csv(connectance_df, paste0(path, file_name,".csv"))

# # to read them out 
# df <- read.csv(file = paste0(path, file_name,".csv"))
# colnames(df)[1] <- "id"
