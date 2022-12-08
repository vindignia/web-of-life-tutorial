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
conflict_prefer("sql", "dplyr")
conflict_prefer("as_data_frame", "igraph")
#
setwd("~/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/")
source('connect_to_DB.R')
#source("resolve_species.R")
#source("helper.R")

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

dim(tmp) 

tmp %>%  formattable() 


species_taxon_entropy <- rbind(species_taxon_entropy_1,species_taxon_entropy_unidentified)
dim(species_taxon_entropy)
colnames(species_taxon_entropy)


networks_df <- tbl(con, "Networks") %>% as.data.frame()
NetworksTypes_df <- tbl(con, "NetworksTypes")  %>% as.data.frame()
networks_species_df <- tbl(con, "v_networksspecies")  %>% as.data.frame()

tmpn <- networks_species_df %>% join(., networks_df, by = c("networkId" = "networkId", "networkName" = "networkName") ) %>% 
  join(., NetworksTypes_df, by = c("networkTypeId" = "networkTypeId")) %>%
  select(specieId, networkId, networkTypeId, networkName, networkTypeName)  


colnames(tmpn)

species_taxon_entropy_df <- species_taxon_entropy %>% left_join(., tmpn, by = c("specieId" = "specieId")) %>%
select(specieId, specieName, kingdomId, orderId, familyId, taxonResolution, taxonUncertainity,
      numOfLevels, totalNumberSpecies, species_entropy, level_entropy, networkId, networkTypeId, networkName, networkTypeName)  %>% 
 

# TODO manage to arrange data so that a shape with increasing/decreasing species_entropy emerges in the bar plot 

colnames(species_taxon_entropy_df)

df <- filter(species_taxon_entropy_df, networkTypeId==7) %>% 
  select(specieId, specieName, species_entropy, level_entropy) 

ggplot(data = df, aes(x=specieName, y=species_entropy)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal()
