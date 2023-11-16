# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)

# dir
dir <- here()
base_dir <- here("belize_data", "step5") 
setwd(base_dir)

temp <- list.files(pattern="*.csv")

#read
dfs <- lapply(temp, read.csv)

#bind
species_param <- do.call(rbind, dfs) %>% 
  select(species, param, est)

#wide
species_param_wide <- species_param %>%
  pivot_wider(names_from = param, values_from = est)

#save
setwd(dir)
write.csv(species_param_wide, file=file.path(here("belize_data", "step7"), "species_param_wide.csv"), row.names = F)