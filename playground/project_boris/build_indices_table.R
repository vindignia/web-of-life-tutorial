library(formattable)
library(dplyr)
library(data.table)
library(ggplot2)

#source("./functions/helper.R")

path = "~/web-of-life-tutorial/data/"
file_name_mod = "modularity_2022-06-24"
file_name_nest = "nestedness_2022-06-21"
file_name_conn = "connectance"
file_name_metrics = "metrics_2023-12-13"

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

#NEW Leandro
metrics_df <- read.csv(file = paste0(path, file_name_metrics,".csv"))
colnames(metrics_df)[1] <- "id"
# we handle this with an inner join with old dataframe
#metrics_df <- mutate(metrics_df, 
#                        network_type =  str_sub(metrics_df$network_name, start = 1L, end = 4L)) 


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

colnames(df)

df <- nestedness_df %>% inner_join(.,connectance_df,  by = c("network_name" = "network_name")) %>% 
  inner_join(.,modularity_df,  by = c("network_name" = "network_name")) %>% 
  inner_join(.,metrics_df,  by = c("network_name" = "network_name"))  %>%  
  select("id.x", 
         "network_name",
         "network_type",
         "num_resources.x",        
         "num_consumers.x",  
         "n_sp",
         #"network_size_ratio", 
         "nestedness_bascompte",  
         #"nestedness_weighted",    
         "nestedness_temperature",
         "connectance",
         "modularity_fast_greedy",      
         "modularity_edge_betweenness", 
         "modularity_leading_eigen", 
         "components",       
         "path_density",
         "leading_eigen",    
         "second_leigen",   
         "spectral_gap",
         #"eigenratio",       
         "lap_firsteigen",   
         "lap_seceigen",    
         #"norm_spectralgap", 
         #"avg_spt",         
         "max_degree",       
         "average_degree",   
         "dif_degree",       
         "var_degree")  %>% 
  rename(id = id.x,
         num_resources = num_resources.x,        
         num_consumers = num_consumers.x,
         num_species = n_sp,
         num_components = components, 
         nestedness = nestedness_bascompte,
         first_eig = leading_eigen,
         second_eig = second_leigen,
         lap_first_eig = lap_firsteigen,   
         lap_second_eig = lap_seceigen,
         ave_degree = average_degree,
         
         )

dim(df)
df %>% formattable()

######### SAVE data #########
path = "~/web-of-life-tutorial/playground/project_boris/"
file_name = "eco_NW_features_2023-12-13"

# save RData objects  
save(nestedness_df, file = paste0(path, file_name,".RData"))

# write file to csv
write.csv(nestedness_df, paste0(path, file_name,".csv"))

