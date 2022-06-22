library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(data.table)
library(bipartite) 

base_url <- "https://www.web-of-life.es/" 
source("./functions/compute_nestedness.R")
source("./functions/change_representation.R")

# download all pollination networks
json_url <- paste0(base_url,"get_networks.php") 
all_nws <- jsonlite::fromJSON(json_url)
head(all_nws)

nw_names <- distinct(all_nws, network_name) %>% 
  dplyr::filter(., !(network_name %like% "FW_")) %>%  # all apart from food-webs
  dplyr::filter(., !(network_name %like% "M_AF_002")) 

# initialize dataframe to store results 
nestedness_df <- NULL         

nw_list <- nw_names$network_name
#nw_list <- c("A_PH_007","M_AF_001") #,"M_SD_016", "A_PH_005",  "M_PA_003")
# for (nw_name in nw_list){
#   print(nw_name)
# }

for (nw_name in nw_list){
  
  # nw_name <- "M_PA_001" 
  nw <- filter(all_nws, network_name == nw_name) 
  
  # select the 3 relevant columns and create the igraph object 
  nw_graph <- nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = FALSE)
  
inc_matrix <- from_wol_graph_to_incidence_matrix(base_url, nw_name, nw_graph, inc_matrix)

# Binarize the matrix 
B <- as.matrix((inc_matrix>0))
class(B) <- "numeric"
inc_matrix <-B 

nw_prop_bipartite <- networklevel(inc_matrix, 
                                  index=c("weighted nestedness","nestedness","weighted NODF"), 
                                  SAmethod="log")

resources_num <- nrow(inc_matrix) 
consumers_num <- ncol(inc_matrix)

nestedness_row <- data.frame(nw_name, 
                             resources_num, 
                             consumers_num, 
                             compute_nestedness(inc_matrix),
                             nw_prop_bipartite[["weighted nestedness"]],
                             nw_prop_bipartite[["nestedness"]],
                             nw_prop_bipartite[["weighted NODF"]])
   
nestedness_df <- rbind(nestedness_df, nestedness_row)

print(paste0("nestedness of ", nw_name, " network"))

}

colnames(nestedness_df) <- NULL
colnames(nestedness_df) <- c("network_name", 
                             "num_resources", 
                             "num_consumers",
                             "nestedness_bascompte",
                             "nestedness_weighted",
                             "nestedness_temperature",
                             "weighted_NODF")

nestedness_df %>% formattable()

######### SAVE data #########
path = "~/web-of-life-tutorial/data/"
file_name = "nestedness_2022-06-21"

# save RData objects  
save(nestedness_df, file = paste0(path, file_name,".RData"))

# write file to csv
write.csv(nestedness_df, paste0(path, file_name,".csv"))

objects()

