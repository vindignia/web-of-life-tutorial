library(igraph)
library(rjson) 
library(formattable)
library(dplyr)
library(bipartite) 

base_url <- "https://www.web-of-life.es/" 
source("./functions/compute_nestedness.R")
source("./functions/change_representation.R")

# download all pollination networks
json_url <- paste0(base_url,"get_networks.php?interaction_type=Pollination") 
pol_nws <- jsonlite::fromJSON(json_url)
head(pol_nws)

pol_nw_names <- distinct(pol_nws, network_name)

# initialize dataframe  to store results 
nestedness_df <- NULL         

for (nw_name in pol_nw_names$network_name){
  
  nw <- filter(pol_nws, network_name == nw_name) 
  
  # select the 3 relevant columns and create the igraph object 
  my_graph <- nw %>% select(species1, species2, connection_strength) %>% 
    graph_from_data_frame(directed = FALSE)
  
inc_matrix <- from_wol_graph_to_incidence_matrix(base_url, nw_name, my_graph, inc_matrix)


nw_prop_bipartite <- networklevel(inc_matrix, 
                                  index=c("weighted nestedness"), 
                                  SAmethod="log")


resources_num <- nrow(inc_matrix) 
consumers_num <- ncol(inc_matrix)
nestedness_row <- data.frame(nw_name, 
                             resources_num, 
                             consumers_num, 
                             compute_nestedness(inc_matrix),
                             nw_prop_bipartite["weighted nestedness"])
   
nestedness_df <- rbind(nestedness_df, nestedness_row)

print(paste0("nestedness of ", nw_name, " network"))

}

rownames(nestedness_df) <- NULL 
colnames(nestedness_df) <- NULL
colnames(nestedness_df) <- c("id",
                             "network_name", 
                             "num_resources", 
                             "num_consumers",
                             "nestedness",
                             "bipartite_nestedness")


nestedness_df <- nestedness_df %>% mutate(id = row_number())
nestedness_df <- nestedness_df[, c(6,1,2,3,4,5)]
nestedness_df %>% formattable()

# Visualize results 

library(ggplot2)

options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  ggtitle("Pollination networks") +
#  stat_function(fun = function(x) power_decay(x,4.8,0.5), color ="black", linetype = "dotted") +
  geom_point(data = nestedness_df, aes(id, nestedness), color = "purple", shape=1) +
  geom_point(data = nestedness_df, aes(id, bipartite_nestedness), color = "black", shape=1) +
  labs(x = "network ID", 
       y = "Network nestedness") +
  xlim(30,50)
  # scale_x_continuous(trans='log10') +
  # scale_y_continuous(trans='log10') 

# vs size
ggplot() +
  ggtitle("Pollination networks") +
  #  stat_function(fun = function(x) power_decay(x,4.8,0.5), color ="black", linetype = "dotted") +
  geom_point(data = nestedness_df, aes(num_resources+num_consumers, nestedness), color = "purple", shape=1) +
  #geom_point(data = nestedness_df, aes(num_resources+num_consumers, bipartite_nestedness), color = "black", shape=1) +
  labs(x = "network size", 
       y = "Network nestedness")
