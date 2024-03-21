###### jOIN FILES
###### Code by Mauricio Collado

###### Compare progress for each harvest SUM limit
#####

# erase
rm(list = ls(all = TRUE)) 


# packages
library(tidyverse)
library(here)
library(scales)
library(gridExtra)
library(grid)
library(kableExtra)
library(ggtext)
library(ggplot2)
library(ggrepel)
library(metR)

# Where to save datasets
fileplace <- "3 Belize"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "all_results"


########################################################################
########################################################################
# read files

#directory 
base_dir <- here(fileplace, fileplace1, "results") 

# temp to list files
temp = list.files(path=base_dir, pattern="*.csv", full.names = TRUE)

# temp to list names
temp1 = list.files(path=base_dir, pattern="*.csv")

# read it all!
for (i in 1:length(temp)) assign(temp1[i], read.csv(temp[i]))

#bind each dataset

# dataset
df <- bind_rows(mget(temp1), .id = "id") 

df_col <- df %>% 
  mutate(id = as.character(gsub("\\D", "", id)))
#  mutate(id = gsub(".*[_]([^.]+)[.]_*", "\\1", id)) %>% 
# mutate(id = substr(id, 1, 3))


result_basket <- df_col %>% 
  select(-id)

# correct some names
result_basket$species[result_basket$species == "cubera snapper"] = "Cubera snapper"
result_basket$species[result_basket$species == "bay snook"] = "Bay snook"
result_basket$species[result_basket$species == "snook"] = "Snook"
result_basket$species[result_basket$species == "black-eyed catfish"] = "Black-eyed catfish"


#max_test1 <- max(result_basket$exploitation.rate, na.rm=T)

write.table(result_basket, here(fileplace, "all_results","results", "final", "result_basket.csv"),
            row.names=FALSE, sep=",")