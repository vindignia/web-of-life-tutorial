library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)
# conflict solver
conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")
#

setwd("~/web-of-life-tutorial/playground/")
#setwd("~/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/playground/")

path = "~/web-of-life-tutorial/data/"
#path = "~/Documents/UZH_icloud/tutorial_project/web-of-life-tutorial/data/"
filename = "species_entropy_2022-12-09.RData"

load(file=paste0(path,filename))

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


# 
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

