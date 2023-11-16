# Step 3 get some useful tables
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
folder4 <- "step3"

data_orig <- read.csv(here(folder1, folder3, "saup_belize_catch_data.csv"), as.is = T, na.strings="")

# Read species
spp_key <- readxl::read_excel((here(folder1, folder2, "unctad_belize_fish.xlsx")))


## Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year, reporting_status) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

# Build order key
order_key <- data %>% 
  # Total by year
  group_by(sci_name, comm_name, name_label, year) %>% 
  summarise(catch_mt=sum(catch_mt)) %>% 
  # Average across years
  group_by(sci_name, comm_name, name_label) %>% 
  summarise(catch_mt_avg=mean(catch_mt, na.rm=T)) %>% 
  # Arrange 
  arrange(desc(catch_mt_avg))

# basket

basket_key <- merge(order_key, spp_key, by="sci_name")

# Order data
data_ordered <- data %>% 
  mutate(name_label=factor(name_label, levels=order_key$name_label))

# catch
data_catch <-  data %>% 
  # Total by year
  group_by(sci_name, comm_name, name_label, year) %>% 
  summarise(catch_mt=sum(catch_mt))

# gear
data_gear <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year, gear_type) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

order_gear <- data_gear %>% 
  # Total by year
  group_by(sci_name, comm_name, name_label, year,  gear_type) %>% 
  summarise(catch_mt=sum(catch_mt)) %>% 
  # Average across years
  group_by(sci_name, comm_name, name_label,  gear_type) %>% 
  summarise(catch_mt_avg=mean(catch_mt, na.rm=T)) %>% 
  # Arrange 
  arrange(desc(catch_mt_avg))

# Save data
write.csv(data_catch ,here(folder1, folder4, "belize_data_catch.csv"), row.names = F)

write.csv(order_gear, here(folder1, folder4, "belize_gear_catch.csv"), row.names = F)

