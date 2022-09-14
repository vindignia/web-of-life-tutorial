library(devtools)
library(rjson)
library(dplyr)
library(formattable)
library(igraph)
library(ggplot2)
library(latex2exp)


# LOAD EXTERNAL PACKAGES
# PACKAGE FROM vindigna github 
remove.packages("weboflife")
devtools::install_github("bascompte-lab/weboflife", force=TRUE)

# https://blog.r-hub.io/2019/12/12/internal-functions/

## Secondary extinction curves

base_url <- "https://www.web-of-life.es/"

my_nw_name <- "M_PL_056" # "M_PL_015" # "M_PL_044" # "M_PL_054"
json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
my_nw <- jsonlite::fromJSON(json_url)

# select the 3 relevant columns and create the igraph object
my_graph <- my_nw %>% select(species1, species2, connection_strength) %>%
  graph_from_data_frame()

#library(weboflife)
my_inc_mat <- weboflife::incidence_matrix_from_graph(my_graph)

# you may compare the dimensions of this matrix with the number of species on the web interface
dim(my_inc_mat)

# We can now use the function `remove_rows(bipartite_network, n_iter, strategy, i_seed)` to compute secondary extinction curves for the three removal strategies defined above. The function takes the following arguments:


iterations <- 15
strategy_vec <- c("RND","MTL","LTM")
df_exp <- NULL
for(removal_strategy in strategy_vec){
  df_exp <- rbind(df_exp, weboflife::remove_rows(my_inc_mat, iterations, removal_strategy, 645) %>%
                    mutate("network_name" = my_nw_name, "strategy" = removal_strategy))
}

# It is good practice to have a look at the dataframe (remove head() in your script)

formattable(head(df_exp))

length(colnames(df_exp))

ggplot() +
  ggtitle(paste0("experiments M_PL_056")) +
  geom_point(data = filter(df_exp, strategy=="RND", network_name=="M_PL_056"), aes(removed_rows/365, (1- removed_columns/91)),
             color = "black",shape = 2) +
  geom_point(data = filter(df_exp, strategy=="LTM", network_name=="M_PL_056"), aes(removed_rows/365, (1- removed_columns/91)),
             color = "dark green",shape = 2) +
  geom_point(data = filter(df_exp, strategy=="MTL", network_name=="M_PL_056"), aes(removed_rows/365, (1- removed_columns/91)),
             color = "red",shape = 2) +
  xlab(TeX("$f_{rows}$")) +
  ylab(TeX("$f_{columns}$"))



base_url <- "https://www.web-of-life.es/"

nw_list <-list("M_PL_015","M_PL_044","M_PL_054","M_PL_056")

rows_removed_vec <- c()
expected_vec <- c()


for (my_nw_name in nw_list) {
  json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
  my_nw <- jsonlite::fromJSON(json_url)

  my_graph <- my_nw %>% select(species1, species2, connection_strength) %>%
    graph_from_data_frame()
  my_inc_mat <- incidence_matrix_from_graph(my_graph)

  iterations <- 5
  removal_strategy <- "RND" # "MTL" # "LTM"
  df <- remove_rows(my_inc_mat, iterations, removal_strategy, 145) %>%
    mutate("network_name" = my_nw_name, "strategy" = removal_strategy)

  rows_removed <- tail(df,2)[1,]$removed_rows

  rows_removed_vec <- append(rows_removed_vec, rows_removed)
  expected_vec <- append(expected_vec, nrow(my_inc_mat))

}

print(rows_removed_vec)
print(expected_vec)
