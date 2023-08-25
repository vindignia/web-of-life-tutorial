library(rjson) 
library(dplyr)
library(igraph)
#library(rweboflife)
library(ggplot2)
library(latex2exp)
library(formattable)
#source("~/ecological_networks_2023/downloads/Functions/helper.R")

k_distribution <- function(my_graph) {
  deg_dist <- my_graph %>% degree_distribution()
  data <- as.data.frame(cbind(seq(0,length(deg_dist)-1), deg_dist)) 
  names(data) <- c("k", "frequency")
  return(data)
}



# download the network from the web-of-life API
base_url <- "https://www.web-of-life.es/"     
json_url <- paste0(base_url,"get_networks.php?network_name=M_PL_015") # M_PL_057
my_nw <- jsonlite::fromJSON(json_url)
# create a graph (meaning igraph object)
my_graph <- my_nw %>% select(species1, species2, connection_strength) %>% 
  graph_from_data_frame() 


df <- k_distribution(my_graph)
# data_ct <- data_ct %>% mutate(k_i = k*frequency)
# k_ave_ct <- sum(data_ct$k_i) 
# k_ave_ct
p <- ggplot() +
  ggtitle("degree dist") +
  geom_point(data = df, aes(k, frequency), color = "purple",shape = 1) +
  # Add segments
  #annotate("segment", x = k_ave_ct, xend = 3, y = 0, yend = 0.5, colour = "black", size=1, alpha=0.5) +
  # Add text
  ylab(TeX("$P(k)$")) + 
  xlab(TeX("$k$"))

print(p)
 