library(tidyverse)
library(dplyr)
library (plyr)
library(data.table)
library(formattable)
# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
# conflict solver
conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("sql", "dplyr")
conflict_prefer("as_data_frame", "igraph")
#
setwd("~/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/")
source('connect_to_DB.R')

#path = "~/web-of-life-tutorial/data/"
path = "~/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/data/"
filename = "mv_species_taxonomy_2022-12-21.RData"

 dbListTables(con)

# ------- select meaningful "Unidentified" Species (the others have no meaningful classification) 
species_taxonomy_unidentified <- tbl(con, "mv_species_taxonomy") %>% 
  dplyr::filter(specieName %like% "Unidentified%" & (is.na(familyId) | is.na(orderId)))  %>% 
  as.data.frame()
dim(species_taxonomy_unidentified)

species_taxon_entropy_unidentified <- species_taxonomy_unidentified %>% 
  mutate(species_entropy = log(taxonUncertainity)/log(totalNumberSpecies), 
         level_entropy = (log(totalNumberSpecies/numOfLevels))/log(totalNumberSpecies))

# ------------------------------------------------------------------------
# ------- select species not named as "Unidentified"

species_taxonomy <- tbl(con, "mv_species_taxonomy") %>% dplyr::filter(!(specieName %like% "Unidentified%")) %>%
  as.data.frame()
dim(species_taxonomy)
colnames(species_taxonomy)

species_taxon_entropy_1 <- species_taxonomy %>% 
  mutate(species_entropy = log(taxonUncertainity)/log(totalNumberSpecies), 
         level_entropy = (log(totalNumberSpecies/numOfLevels))/log(totalNumberSpecies))

# ------------------------------------------------------------------------

tmp <- species_taxon_entropy_unidentified %>% filter(species_entropy >= 0.8 & species_entropy <=1.)
tmp <- species_taxon_entropy_1 %>% filter(species_entropy >= 0.8 & species_entropy <=1.)


species_taxon_entropy <- rbind(species_taxon_entropy_1,species_taxon_entropy_unidentified)
dim(species_taxon_entropy)
colnames(species_taxon_entropy)


networks_df <- tbl(con, "Networks") %>% as.data.frame()
NetworksTypes_df <- tbl(con, "NetworksTypes")  %>% as.data.frame()
networks_species_df <- tbl(con, "v_networksspecies")  %>% as.data.frame()

tmpn <- networks_species_df %>% join(., networks_df, by = c("networkId" = "networkId", "networkName" = "networkName") ) %>% 
  join(., NetworksTypes_df, by = c("networkTypeId" = "networkTypeId")) %>%
  select(specieId, networkId, networkTypeId, networkName, networkTypeName)  


colnames(networks_df)
colnames(tmpn)

species_taxon_entropy_df <- species_taxon_entropy %>% left_join(., tmpn, by = c("specieId" = "specieId")) %>%
select(specieId, specieName, kingdomId, orderId, familyId, taxonResolution, taxonUncertainity,
      numOfLevels, totalNumberSpecies, species_entropy, level_entropy, networkId, networkTypeId, networkName, networkTypeName)   
 
colnames(species_taxon_entropy_df)

# save the whole materialized view 
# mv_species_taxonomy <- tbl(con, "mv_species_taxonomy") %>% as.data.frame()
# save(mv_species_taxonomy, file=paste0(path,filename))

#save(species_taxon_entropy_df, file=paste0(path,filename))


# Build df for the bar plots 
df <- filter(species_taxon_entropy_df, networkTypeId==7) %>% 
  arrange(species_entropy) 
df <- cbind(df, data.frame(count = 1:nrow(df)))
FW_df <- df %>% mutate(species_fraction=count/nrow(df)) 

df <- filter(species_taxon_entropy_df, networkTypeId==5) %>% 
  #select(specieId, specieName, species_entropy, level_entropy) %>%
  arrange(species_entropy) 
df <- cbind(df, data.frame(count = 1:nrow(df)))
PL_df <- df %>% mutate(species_fraction=count/nrow(df)) 

df <- filter(species_taxon_entropy_df, networkTypeId==6) %>% 
  #select(specieId, specieName, species_entropy, level_entropy) %>%
  arrange(species_entropy) 
df <- cbind(df, data.frame(count = 1:nrow(df)))
SD_df <- df %>% mutate(species_fraction=count/nrow(df)) 

df <- FW_df
df <- rbind(df, PL_df)
df <- rbind(df, SD_df)
df <- df %>% select(specieId, specieName, species_entropy, level_entropy, networkTypeName, count, species_fraction)
#df <- df %>% rename("network_type" = networkTypeName)

dim(df)
dim(distinct(df,specieId))
nrow(df)
colnames(df)
df %>% formattable()

ggplot(df, aes(x=count, y=species_entropy, fill=networkTypeName)) +
  geom_bar(stat="identity", alpha = 0.5)   

# Normalized plots 
# PL 
PL_df1 <- PL_df %>% select(species_fraction,species_entropy, level_entropy) %>%  
  gather(Legend, Entropy, -species_fraction)
ggplot(PL_df1, aes(x=species_fraction, y=Entropy, fill=Legend)) +
  geom_bar(stat="identity", alpha = 0.5, position = "dodge") + 
  scale_fill_manual("legend", values = c("species_entropy" = "#1E8449", "level_entropy" = "#9B59B6")) + 
  ggtitle("Pollination") 


# SD 
SD_df1 <- SD_df %>% select(species_fraction,species_entropy, level_entropy) %>%  
  gather(legend, Entropy, -species_fraction)
ggplot(SD_df1, aes(x=species_fraction, y=Entropy, fill=legend)) +
  geom_bar(stat="identity", alpha = 0.5, position = "dodge") + 
  scale_fill_manual("legend", values = c("species_entropy" = "#2E86C1", "level_entropy" = "#9B59B6")) + 
  ggtitle("Seed Dispersal") 

# FW 
FW_df1 <- FW_df %>% select(species_fraction,species_entropy, level_entropy) %>%  
  gather(legend, Entropy, -species_fraction)
ggplot(FW_df1, aes(x=species_fraction, y=Entropy, fill=legend)) +
  geom_bar(stat="identity", alpha = 0.5, position = "dodge") + 
  scale_fill_manual("legend", values = c("species_entropy" = "#D35400", "level_entropy" = "#9B59B6")) + 
  ggtitle("Food Webs") 


# TODO group_by networkId and networkTypeId and summarize by networkId (or name)   
