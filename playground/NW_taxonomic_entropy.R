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
filename = "mv_species_taxonomy_2022-12-21.RData" # df of the materialized view mv_species_taxonomy

load(file=paste0(path,filename))
class(mv_species_taxonomy)
head(mv_species_taxonomy,3)

# LOOK at taxonomic_entropy.R file to see how data is filtered 
# TODO join with network information fecthed from the development website 
# compute the entropy per network (see point 1. and *qmd document)