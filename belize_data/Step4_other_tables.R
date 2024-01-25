
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)

#read
folder1 <- "belize_data"
folder2 <- "step1"
folder3 <- "step4"

#data_orig <- read_csv(here("processed", "belize_data_catch.csv"))
data_orig <- readRDS(here(folder1, folder2, "saup_belize_catch_data"))
                     
# Read species
#spp_key <- read.csv("belize_data/step2/belize_fish_life_history.csv", as.is=T) 
spp_key <- read.csv(here("belize_data", "step2", "belize_fish_life_history.csv"), as.is=T)   

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year, reporting_status, gear_type) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

# Build order key
order_key <- data %>% 
  # Total by year
  group_by(sci_name, comm_name, name_label, year) %>% 
  summarise(catch_mt=sum(catch_mt),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  # Average across years
  group_by(sci_name, comm_name, name_label) %>% 
  summarise(catch_mt_avg=mean(catch_mt, na.rm=T),
            value_usd_avg=mean(value_usd, na.rm=T)) %>% 
  # Arrange 
  arrange(desc(catch_mt_avg)) 


### mixing reporting status
data1 <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year, gear_type) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

# order data
data_ordered <- data1 %>% 
  mutate(name_label=factor(name_label, levels=order_key$name_label))

# price proxies
price_key <- order_key %>% 
  mutate(value_mt=value_usd_avg/catch_mt_avg,
         value_pound=round(value_mt/2204.62, 2))

#total catch per year
catch_year <- data_orig %>% 
  # Reduce to species of interest
  filter(sci_name %in% spp_key$sci_name) %>% 
  # Summarize
  group_by(sci_name, comm_name, year) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Build name label
  mutate(name_label=paste0(comm_name, "\n(", sci_name, ")"))

###### save everything
write.csv(catch_year , file=file.path(here("belize_data", "step4"), "belize_catch_year1.csv"), row.names = F)
write.csv(price_key , file=file.path(here("belize_data", "step4"), "belize_price_key.csv"), row.names = F)
write.csv(data_ordered, file=file.path(here("belize_data", "step4"), "belize_data_ordered.csv"), row.names = F)
