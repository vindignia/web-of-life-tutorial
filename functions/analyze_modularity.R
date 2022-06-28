library(formattable)
library(dplyr)
library(data.table)
library(ggplot2)

source("./functions/helper.R")

path = "~/web-of-life-tutorial/data/"
file_name_mod = "modularity_2022-06-24"
file_name_nest = "nestedness_2022-06-21"
file_name_conn = "connectance"

connectance_df <- read.csv(file = paste0(path, file_name_conn,".csv"))
colnames(connectance_df)[1] <- "id"
connectance_df <- mutate(connectance_df, 
                        network_type =  str_sub(connectance_df$network_name, start = 1L, end = 4L)) 

nestedness_df <- read.csv(file = paste0(path, file_name_nest,".csv"))
colnames(nestedness_df)[1] <- "id"
nestedness_df <- mutate(nestedness_df, 
                        network_size_ratio = nestedness_df$num_resources/nestedness_df$num_consumers, 
                        network_type = str_sub(nestedness_df$network_name, start = 1L, end = 4L)) 

modularity_df <- read.csv(file = paste0(path, file_name_mod,".csv"))
colnames(modularity_df)[1] <- "id"
modularity_df <- mutate(modularity_df, 
                        network_type =  str_sub(modularity_df$network_name, start = 1L, end = 4L)) 

# check the actual size of our samples 
type_list <- distinct(nestedness_df,network_type)$network_type 
networks_per_type <- NULL 
for(nw_type in type_list){
  row <- data.frame(nw_type, filter(nestedness_df,network_type == nw_type) %>% nrow())
  networks_per_type <- rbind(networks_per_type, row)
}
rownames(networks_per_type) <- NULL 
colnames(networks_per_type) <- NULL
colnames(networks_per_type) <- c("network_type", "count")
networks_per_type


analyzed_type_list <- c("A_HP","M_PL","M_SD")

nestedness_df <- filter(nestedness_df, network_type %in% analyzed_type_list)
connectance_df <- filter(connectance_df, network_type %in% analyzed_type_list)
modularity_df <- filter(modularity_df, network_type %in% analyzed_type_list)

nestedness_df %>% formattable()
connectance_df %>% formattable()
modularity_df %>% formattable()




###################################################
# HISTOGRAMS MODULARITY VALUES
# tutorial https://www.youtube.com/watch?v=FzfE8tfbpvQ
# counts 
ggplot(modularity_df, aes(x=modularity_fast_greedy, color=network_type, fill=network_type)) +
#  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlim(0, 1)  # alpha is the fill transparency

ggplot(modularity_df, aes(x=modularity_edge_betweenness, color=network_type, fill=network_type)) +
  #geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgrey")  
  geom_density(alpha = 0.2) + xlim(0, 1)

ggplot(modularity_df, aes(x=modularity_leading_eigen, color=network_type, fill=network_type)) +
  #geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightblue")  
  geom_density(alpha = 0.2) + xlim(0, 1)


###################################################
# HISTOGRAMS NESTEDNESS VALUES
# tutorial https://www.youtube.com/watch?v=FzfE8tfbpvQ
# counts 
ggplot(nestedness_df, aes(x=nestedness_bascompte, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlim(0, 1)  # alpha is the fill transparency


###################################################
# HISTOGRAMS CONNECTANCE VALUES
# tutorial https://www.youtube.com/watch?v=FzfE8tfbpvQ
# counts 
ggplot(connectance_df, aes(x=connectance, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlim(0, 1)  # alpha is the fill transparency


###################################################
# HISTOGRAMS SIZE VALUES
# tutorial https://www.youtube.com/watch?v=FzfE8tfbpvQ
# counts 
ggplot(nestedness_df, aes(x=(num_resources+num_consumers), color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + 
  xlab("network size") + 
  xlim(0, 500)  # alpha is the fill transparency


ggplot(nestedness_df, aes(x=network_size_ratio, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlab("resources/consumers")

  #+ xlim(0, 500)  # alpha is the fill transparency

