library(rjson) 
library(formattable)
library(dplyr)
library(tidyverse)
library(igraph)

path_to_folder <- "~/web-of-life-tutorial/functions/"
source(paste0(path_to_folder, "leandro_metrics.R"))

# define the base url
base_url <- "https://www.web-of-life.es/"     
json_url <- paste0(base_url,"get_networks.php") 
all_nws <- jsonlite::fromJSON(json_url)
head(all_nws)

nw_names <- distinct(all_nws, network_name) %>% 
  dplyr::filter(!str_detect(network_name, "FW_")) %>% # all apart from food-webs
  dplyr::filter(!str_detect(network_name, "M_AF_002")) 

tail(nw_names)

nw_list <- nw_names$network_name

# initialize dataframe to store results 
metrics_df <- NULL      

for (nw_name in nw_list){
  
  # nw_name <- "M_PA_001" 
  nw <- filter(all_nws, network_name == nw_name) 
  
  # select the 3 relevant columns and create the igraph object   
  nw_graph <- nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = FALSE)
  
  nw_adj_mat <- as_adjacency_matrix(nw_graph, 
                                    #type = "lower", 
                                    attr = "connection_strength",
                                    sparse = FALSE)  
  class(nw_adj_mat) <-"numeric"
  # remove NA values (previous empty strings)
  nw_adj_mat[which(is.na(nw_adj_mat) == T)]<-0
  # check dimensions and compare with the number of species on the Web interface 
  dim(nw_adj_mat)
  
  nw_bin_mat <- nw_adj_mat>0 
  
  class(nw_bin_mat) <-"numeric"
  
  metrics_row <- data.frame(nw_name, nw_metrics(nw_bin_mat))
  metrics_df <- rbind(metrics_df, metrics_row)
}

metrics_df <- rename(metrics_df, network_name = nw_name)


metrics_df %>% formattable()


######### SAVE data #########
path = "~/web-of-life-tutorial/data/"
file_name = "metrics_2023-12-13"

# save RData objects  
save(metrics_df, file = paste0(path, file_name,".RData"))

# write file to csv
write.csv(metrics_df, paste0(path, file_name,".csv"))
