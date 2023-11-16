# Step 1: life history data
################################################################################
### I DIDNT ADJUSTED THIS FILE!


# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)

# Directories
indir <- "belize_data/raw"
outdir <- "belize_data/step2"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "unctad_belize_fish.xlsx"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Simplify
  select(comm_name, sci_name) %>% 
  # Correct sci names
  mutate(sci_name=recode(sci_name, 
                         "Cichlasoma synspilum"="Vieja melanurus",  
                         "Cichlosomas urphthalmus"="Mayaheros urophthalmus", 
                         "Haemulon plumieri"="Haemulon plumierii",       
                         "Haemulun parra"="Haemulon parra"))

# Check names
freeR::check_names(data$sci_name)

# Get FL life history data
data_lh <- freeR::fishlife(species = data$sci_name)

# Add common name
data_lh1 <- data_lh %>% 
  left_join(data, by=c("sci_name")) %>% 
  select(sci_name, comm_name, everything())

# Export
write.csv(data_lh1, file=file.path(outdir, "belize_fish_life_history.csv"), row.names = F)