# Step 1 clean data from SAU
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)

# Read data
folder1 <- "belize_data"
folder2 <- "raw"
folder3 <- "step1"

data_orig <- read.csv(here(folder1, folder2, "SAU EEZ 84 v50-1.csv"), as.is = T, na.strings="")

# Read species
spp_do <- readxl::read_excel(here(folder1, folder2, "unctad_belize_fish.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(sci_name=scientific_name,
         comm_name=common_name,
         catch_mt=tonnes,
         value_usd=landed_value) %>% 
  # Remove useless columns
  select(-c(area_name, area_type))

# Inspect data
str(data)

# Check species names
#spp_key <- data %>% 
#  select(sci_name, comm_name, functional_group, commercial_group) %>% 
#  unique()
#freeR::which_duplicated(spp_key$sci_name)
#freeR::check_names(spp_key$sci_name)


# Export data
################################################################################

# Export
write.csv(data, file=here(folder1, folder3, "saup_belize_catch_data.csv"), row.names = F)

saveRDS(data, file=here(folder1, folder3, "saup_belize_catch_data.csv"))


