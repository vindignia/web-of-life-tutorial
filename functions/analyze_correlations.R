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

# nestedness_df %>% formattable()
# connectance_df %>% formattable()
# modularity_df %>% formattable()


df <- nestedness_df %>% inner_join(.,connectance_df,  by = c("network_name" = "network_name")) %>% 
  inner_join(.,modularity_df,  by = c("network_name" = "network_name")) %>% 
  select("id", 
         "network_name",
         "num_resources.x",        
         "num_consumers.x",   
         "network_size_ratio", 
         "nestedness_bascompte",  
         "nestedness_weighted",    
         "nestedness_temperature",
         "connectance",
         "modularity_fast_greedy",      
         "modularity_edge_betweenness", 
         "modularity_leading_eigen",   
         "network_type")  %>% 
  rename(num_resources = num_resources.x,        
         num_consumers = num_consumers.x)

dim(df)
df %>% formattable()


############### CORRELATIONS ##############

# Connectance vs NW size aspect ratio

options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  #ggtitle("correlations") +
  geom_point(data =  df, aes(x=network_size_ratio, y=connectance,  color=network_type),  shape=1) +
  labs(x = "resources/consumers", 
       y = "connectance") +
  xlim(0, 1)


# Connectance vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=connectance, color=network_type), shape=1) +
  stat_function(fun = function(x) power_decay(x,7.,0.9), color ="black", linetype = "dotted") +
  labs(x = "network size", 
       y = "connectance") +
  ylim(0, 1) + 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 

# NESTEDNESS
# Nestedness bascompte vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=nestedness_bascompte, color=network_type), shape=1) +
  stat_function(fun = function(x) power_decay(x,7.,0.7), color ="black", linetype = "dotted") +
  labs(x = "network size", 
       y = "nestedness_bascompte") +
  ylim(0, 1) + 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


# nestedness_weighted vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=nestedness_weighted, color=network_type), shape=1) +
  #stat_function(fun = function(x) power_decay(x,7.,0.7), color ="black", linetype = "dotted") +
  labs(x = "network size", 
       y = "nestedness_weighted") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


# nestedness_temperature vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=nestedness_temperature, color=network_type), shape=1) +
  stat_function(fun = function(x) power_decay(x,500.,0.95), color ="black", linetype = "dotted") +
  labs(x = "network size", 
       y = "nestedness_temperature") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


# MODULARITY  
# modularity_fast_greedy	vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=modularity_fast_greedy, color=network_type), shape=1) +
  labs(x = "network size", 
       y = "modularity_fast_greedy") +
  ylim(0, 1) + 
  scale_x_continuous(trans='log10') 


# modularity_edge_betweenness vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=modularity_edge_betweenness, color=network_type), shape=1) +
  labs(x = "network size", 
       y = "modularity_edge_betweenness") +
  ylim(0, 1) + 
  scale_x_continuous(trans='log10') 


# modularity_leading_eigen vs NW size
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=(num_resources+num_consumers), y=modularity_leading_eigen, color=network_type), shape=1) +
  labs(x = "network size", 
       y = "modularity_leading_eigen") +
  ylim(0, 1) + 
  scale_x_continuous(trans='log10') 



# CROSS correlations 

# nestedness_bascompte vs connectance
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=connectance, y=nestedness_bascompte, color=network_type), shape=1) +
  stat_function(fun = function(x) power_decay(x,1.5,-0.8), color ="black", linetype = "dotted") +
  labs(x = "connectance", 
       y = "nestedness_bascompte") + 
  xlim(0, 1) + ylim(0, 1) 


# modularity_leading_eigen  vs connectance
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=connectance, y=modularity_leading_eigen, color=network_type), shape=1) +
  labs(x = "connectance", 
       y = "modularity_leading_eigen") +
  xlim(0, 1) + ylim(0, 1) 


# modularity_leading_eigen  vs nestedness_bascompte
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  geom_point(data =  df, aes(x=nestedness_bascompte, y=modularity_leading_eigen, color=network_type), shape=1) +
  labs(x = "nestedness_bascompte", 
       y = "modularity_leading_eigen") +
  xlim(0, 1) + ylim(0, 1) 

