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
  dplyr::filter(.,!(network_name %like% "FW_")) # filter out food-webs
  dplyr::filter(.,!(network_name == "M_PL_062")) # too large we compute it separately
# problematic "M_PL_061_48"
  
# initialize dataframe to store results 
#modularity_df <- NULL       

#nw_list <- c("A_PH_007","M_AF_001","M_SD_016", "A_PH_005",  "M_PA_003","M_PL_073")
i <-  0 
for (nw_name in nw_list){
  i=i+1 
  print(nw_name)
  print(i)
}

nw_list <-  nw_names[210:264,] # after "M_PL_062"

for (nw_name in nw_list){
  
  
  nw_name <- "M_PL_062" # only "M_PL_062"
  nw <- filter(all_nws, network_name == nw_name) 
  
  print(paste0("modularity of ", nw_name, " network"))

  # select the 3 relevant columns and create the igraph object 
  my_graph <- nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = FALSE)
  
  #inc_matrix <- from_wol_graph_to_incidence_matrix(base_url, nw_name, my_graph, inc_matrix)
  
  # QuaBiMo in bipartite 
  # nw_prop_bipartite <- networklevel(inc_matrix,
  #                                   index=c("number of compartments","compartment diversity"),
  #                                   SAmethod="log")

  modules1 <- cluster_fast_greedy(my_graph)
  modules2 <- cluster_edge_betweenness(my_graph)
  modules3 <- cluster_leading_eigen(my_graph)
  
  mod_row <- data.frame(nw_name,
                        modularity(modules1, resolution=1), 
                        modularity(modules2, resolution=1), 
                        modularity(modules3, resolution=1)
                       )
  
  ################ up to here "M_PL_062" 
  
  modularity_df <- rbind(modularity_df,mod_row)
  
  
}

# rownames(mod_row) <- NULL
# colnames(mod_row) <- NULL
# colnames(mod_row) <- c("nw_name",
#                              "modularity_fast_greedy",
#                              "modularity_edge_betweenness",
#                              "modularity_leading_eigen")


rownames(modularity_df) <- NULL 
colnames(modularity_df) <- NULL

colnames(modularity_df) <- c("nw_name",
                             "modularity_fast_greedy",
                             "modularity_edge_betweenness",
                             "modularity_leading_eigen")

modularity_df %>% formattable()

######### SAVE data #########
path = "~/web-of-life-tutorial/data/"
file_name = "modularity_2022-06-24"

# save RData objects  
save(modularity_df, file = paste0(path, file_name,".RData"))

# write file to csv
write.csv(modularity_df, paste0(path, file_name,".csv"))

# # to read them out 
# df <- read.csv(file = paste0(path, file_name,".csv"))
# colnames(df)[1] <- "id"
# dim(df)
# df %>% formattable() 
