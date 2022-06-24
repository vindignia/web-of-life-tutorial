 
library(formattable)
library(dplyr)
library(data.table)
library(ggplot2)


path = "~/web-of-life-tutorial/data/"
file_name = "nestedness_2022-06-21"

nestedness_df <- read.csv(file = paste0(path, file_name,".csv"))
colnames(nestedness_df)[1] <- "id"

nestedness_df <- mutate(nestedness_df, 
                        network_type =  str_sub(nestedness_df$network_name, start = 1L, end = 4L)) 

nestedness_df %>% formattable()

options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
  ggtitle("nestedness networks") +
  geom_point(data = nestedness_df, aes(id, nestedness), color = "purple", shape=1) +
  geom_point(data = nestedness_df, aes(id, weighted_nestedness), color = "black", shape=1) +
  labs(x = "network ID", 
       y = "Network nestedness") +
  xlim(0,20)
# scale_x_continuous(trans='log10') +
# scale_y_continuous(trans='log10') 

# vs size
ggplot() +
  ggtitle("Bascompte nestedness") +
  #  stat_function(fun = function(x) power_decay(x,4.8,0.5), color ="black", linetype = "dotted") +
  geom_point(data = filter(nestedness_df, network_name %like% "A_HP"), aes(num_resources+num_consumers, nestedness_bascompte), 
             color = "purple", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "A_PH"), aes(num_resources+num_consumers, nestedness_bascompte), 
             color = "black", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PA"), aes(num_resources+num_consumers, nestedness_bascompte), 
             color = "green", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PL"), aes(num_resources+num_consumers, nestedness_bascompte), 
             color = "blue", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_SD"), aes(num_resources+num_consumers, nestedness_bascompte), 
             color = "orange", shape=16, size=2) +
  labs(x = "network size", 
       y = "Network nestedness") + 
  xlim(0,200)


ggplot() +
  ggtitle("weighted nestedness bipartite") +
  #  stat_function(fun = function(x) power_decay(x,4.8,0.5), color ="black", linetype = "dotted") +
  geom_point(data = filter(nestedness_df, network_name %like% "A_HP"), aes(num_resources+num_consumers, nestedness_weighted), 
             color = "purple", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "A_PH"), aes(num_resources+num_consumers, nestedness_weighted), 
             color = "black", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PA"), aes(num_resources+num_consumers, nestedness_weighted), 
             color = "green", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PL"), aes(num_resources+num_consumers, nestedness_weighted), 
             color = "blue", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_SD"), aes(num_resources+num_consumers, nestedness_weighted), 
             color = "orange", shape=16, size=2) +
  labs(x = "network size", 
       y = "Network nestedness") + 
  xlim(0,200)


ggplot() +
  ggtitle("nestedness temperature bipartite") +
  #  stat_function(fun = function(x) power_decay(x,4.8,0.5), color ="black", linetype = "dotted") +
  geom_point(data = filter(nestedness_df, network_name %like% "A_HP"), aes(num_resources+num_consumers, nestedness_temperature), 
             color = "purple", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "A_PH"), aes(num_resources+num_consumers, nestedness_temperature), 
             color = "black", shape=1, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PA"), aes(num_resources+num_consumers, nestedness_temperature), 
             color = "green", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_PL"), aes(num_resources+num_consumers, nestedness_temperature), 
             color = "blue", shape=16, size=2) +
  geom_point(data = filter(nestedness_df, network_name %like% "M_SD"), aes(num_resources+num_consumers, nestedness_temperature), 
             color = "orange", shape=16, size=2) +
  labs(x = "network size", 
       y = "Network nestedness") + 
  xlim(0,200)


###################################################
# HISTOGRAMS NESTEDNESS VALUES
# tutorial https://www.youtube.com/watch?v=FzfE8tfbpvQ
# counts 
ggplot(nestedness_df, aes(x=nestedness_bascompte, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlim(0, 1)  # alpha is the fill transparency

ggplot(nestedness_df, aes(x=nestedness_weighted, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) + xlim(0, 1)  # alpha is the fill transparency

ggplot(nestedness_df, aes(x=nestedness_temperature, color=network_type, fill=network_type)) +
  #  geom_histogram(freq = TRUE, breaks = seq(0,1,0.05), fill="lightgreen")  
  geom_density(alpha = 0.2) #+ xlim(0, 1) 
