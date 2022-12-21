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


