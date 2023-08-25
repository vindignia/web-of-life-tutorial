library(rjson) 
library(dplyr)
library(igraph)
library(ggplot2)
library(latex2exp)
library(formattable)
#source("~/ecological_networks_2023/downloads/Functions/helper.R")

# LOAD EXTERNAL PACKAGES
# PACKAGE FROM vindigna github
# remove.packages("rweboflife")
#detach(package:rweboflife,unload=TRUE)
#devtools::install_github("vindignia/rweboflife", force=TRUE)
#devtools::install_github("bascompte-lab/rweboflife", force=TRUE)
library(rweboflife)


k_distribution <- function(my_graph, label = "my_nw") {
  deg_dist <- my_graph %>% degree_distribution()
  data <- as.data.frame(cbind(seq(0,length(deg_dist)-1), deg_dist)) 
  names(data) <- c("k", "frequency")
  data <- data %>% mutate(nw_name = label) 
  return(data)
}

shuffle_graph <- function(gg, algorithm = 'curveball') {
  
  inc_matrix <- as_incidence_matrix(
    gg,
    attr = "connection_strength",
    names = FALSE,
    sparse = FALSE
  )
  # convert elements into numeric values 
  class(inc_matrix) <-"numeric"
  # remove NA values (previous empty strings)
  inc_matrix[which(is.na(inc_matrix) == T)]<-0
  colnames(inc_matrix) <- NULL
  # dim(inc_matrix)
  # class(inc_matrix)
  
  iter <- ceiling(1.8*ncol(inc_matrix)) 
  M <- null_model(inc_matrix, iter_max=iter, model = algorithm)
  
  return(graph_from_incidence_matrix(M))
}    


# download the network from the web-of-life API
base_url <- "https://www.web-of-life.es/"     
nw_name <- "M_PL_057" # "M_PL_015"  #  

json_url <- paste0(base_url,"get_networks.php?network_name=",nw_name)
my_nw <- jsonlite::fromJSON(json_url)
# create a graph (meaning igraph object)
my_graph <- my_nw %>% select(species1, species2, connection_strength) %>% 
  graph_from_data_frame() 

my_info <- read.csv(paste0(base_url,"get_species_info.php?network_name=",nw_name))
isResource <- my_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE
V(my_graph)$type <- !(isResource)  # Add the "type" attribute
#is_bipartite(my_graph)


cb_graph <- shuffle_graph(my_graph)
sw_graph <- shuffle_graph(my_graph, "swap")
ef_graph <- shuffle_graph(my_graph, "equifrequent")
is_bipartite(ef_graph)


df <- k_distribution(my_graph, nw_name)
df_cb <- k_distribution(cb_graph, nw_name)
df_sw <- k_distribution(sw_graph, nw_name)
df_ef <- k_distribution(ef_graph, nw_name)

# Note that while the swap and curveball models conserve the degree distribution that's not the case for the equifrequent model 
ggplot() +
  ggtitle("degree dist") +
  geom_point(data = filter(df, nw_name == "M_PL_057"), aes(k, frequency), color = "black",shape = 1, size = 2) +
  geom_point(data = filter(df_cb, nw_name == "M_PL_057"), aes(k, frequency), color = "blue",shape = 15, size = 0.8) +
  geom_point(data = filter(df_sw, nw_name == "M_PL_057"), aes(k, frequency), color = "green",shape = 16, size = 0.8) +
  geom_point(data = filter(df_ef, nw_name == "M_PL_057"), aes(k, frequency), color = "red",shape = 18, size = 1.5) +
#  geom_point(data = filter(df, nw_name == "M_PL_057"), aes(k, frequency), color = "purple",shape = 1) +
  # Add segments
  #annotate("segment", x = k_ave_ct, xend = 3, y = 0, yend = 0.5, colour = "black", size=1, alpha=0.5) +
  # Add text
  ylab(TeX("$P(k)$")) + 
  xlab(TeX("$k$")) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


path_to_foder <- "/home/alessandro/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/null_models_convergence/"

#save(df, df_cb, df_sw,df_ef, file =paste0(path_to_foder,"df_deg_dist_2023-08-25.RData"))

  
