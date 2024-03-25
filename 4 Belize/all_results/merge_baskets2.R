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
fileplace1 <- "basket2"
fileplace2 <- "figures"

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

#test
df <- bind_rows(output_0.05_.csv, output_0.1_.csv, .id = "id")

# dataset
df <- bind_rows(mget(temp1), .id = "id") 

df_col <- df %>% 
  mutate(id = as.character(gsub("\\D", "", id)))
#  mutate(id = gsub(".*[_]([^.]+)[.]_*", "\\1", id)) %>% 
# mutate(id = substr(id, 1, 3))


########################################################################
########################################################################
########################################################################
########################################################################

# rep(c(1, 2, 3), times=10)
# seq(0,30)

output_base <- df_col %>% 
  mutate(year = rep(seq(0,30), times=100), # before times = 20
         harbio1= harvest.s_1/lag(stock.s_1),
         harbio2= harvest.s_2/lag(stock.s_2),
         harbio3= harvest.s_3/lag(stock.s_3),
         harbio4= harvest.s_4/lag(stock.s_4),
         harbio5= harvest.s_5/lag(stock.s_5),
         harbio6= harvest.s_6/lag(stock.s_6))

#Group them

all_outputs <- output_base  %>% 
  mutate(tot_profits = profit_per_t.t_1 + profit_per_t.t_2, 
         basket = 2,
         per_quota = case_when(id == "001" ~ "1",
                         id == "002" ~ "2",
                         id == "003" ~ "3",
                         id == "004" ~ "4",
                         id == "005" ~ "5",
                         id == "006" ~ "6",
                         id == "007" ~ "7",
                         id == "008" ~ "8",
                         id == "009" ~ "9",
                         id == "01" ~ "10",
                         id == "011" ~ "11",
                         id == "012" ~ "12",
                         id == "013" ~ "13",
                         id == "014" ~ "14",
                         id == "015" ~ "15",
                         id == "016" ~ "16",
                         id == "017" ~ "17",
                         id == "018" ~ "18",
                         id == "019" ~ "19",
                         id == "02" ~ "20",
                         id == "021" ~ "21",
                         id == "022" ~ "22",
                         id == "023" ~ "23",
                         id == "024" ~ "24",
                         id == "025" ~ "25",
                         id == "026" ~ "26",
                         id == "027" ~ "27",
                         id == "028" ~ "28",
                         id == "029" ~ "29",
                         id == "03" ~ "30",
                         id == "031" ~ "31",
                         id == "032" ~ "32",
                         id == "033" ~ "33",
                         id == "034" ~ "34",
                         id == "035" ~ "35",
                         id == "036" ~ "36",
                         id == "037" ~ "37",
                         id == "038" ~ "38",
                         id == "039" ~ "39",
                         id == "04" ~ "40",
                         id == "041" ~ "41",
                         id == "042" ~ "42",
                         id == "043" ~ "43",
                         id == "044" ~ "44",
                         id == "045" ~ "45",
                         id == "046" ~ "46",
                         id == "047" ~ "47",
                         id == "048" ~ "48",
                         id == "049" ~ "49",
                         id == "05" ~ "50",
                         id == "051" ~ "51",
                         id == "052" ~ "52",
                         id == "053" ~ "53",
                         id == "054" ~ "54",
                         id == "055" ~ "55",
                         id == "056" ~ "56",
                         id == "057" ~ "57",
                         id == "058" ~ "58",
                         id == "059" ~ "59",
                         id == "06" ~ "60",
                         id == "061" ~ "61",
                         id == "062" ~ "62",
                         id == "063" ~ "63",
                         id == "064" ~ "64",
                         id == "065" ~ "65",
                         id == "066" ~ "66",
                         id == "067" ~ "67",
                         id == "068" ~ "68",
                         id == "069" ~ "69",
                         id == "07" ~ "70",
                         id == "071" ~ "71",
                         id == "072" ~ "72",
                         id == "073" ~ "73",
                         id == "074" ~ "74",
                         id == "075" ~ "75",
                         id == "076" ~ "76",
                         id == "077" ~ "77",
                         id == "078" ~ "78",
                         id == "079" ~ "79",
                         id == "08" ~ "80",
                         id == "081" ~ "81",
                         id == "082" ~ "82",
                         id == "083" ~ "83",
                         id == "084" ~ "84",
                         id == "085" ~ "85",
                         id == "086" ~ "86",
                         id == "087" ~ "87",
                         id == "088" ~ "88",
                         id == "089" ~ "89",
                         id == "09" ~ "90",
                         id == "091" ~ "91",
                         id == "092" ~ "92",
                         id == "093" ~ "93",
                         id == "094" ~ "94",
                         id == "095" ~ "95",
                         id == "096" ~ "96",
                         id == "097" ~ "97",
                         id == "098" ~ "98",
                         id == "099" ~ "99",
                         id == "1" ~ "100"))


# PIVOT

species1 <- "White grunt"
species2 <-"Gray snapper"
species3 <-"Bluestriped grunt"
species4 <-"Great barracuda"
species5 <-"Mojarra (yellowfin)"
species6 <-"Mojarra (Pompano)"


basket1_bio <- all_outputs %>% 
  select(basket, per_quota, year, tot_profits, effort.t_1, starts_with("stock")) %>% 
  pivot_longer(cols=starts_with("stock"),
               names_to='species',
               values_to='biomass') %>% 
  mutate(species = case_when(species == "stock.s_1" ~ species1,
                             species == "stock.s_2" ~ species2,
                             species == "stock.s_3" ~ species3,
                             species == "stock.s_4" ~ species4,
                             species == "stock.s_5" ~ species5,
                             species == "stock.s_6" ~ species6))




basket1_har <- all_outputs %>% 
  select(basket, per_quota, year, tot_profits, effort.t_1, starts_with("harvest")) %>% 
  pivot_longer(cols= starts_with("harvest"), # c(6:13),
               names_to='species',
               values_to='harvest') %>% 
  mutate(species = case_when(species == "harvest.s_1" ~  species1,
                             species == "harvest.s_2" ~  species2,
                             species == "harvest.s_3" ~  species3,
                             species == "harvest.s_4" ~  species4,
                             species == "harvest.s_5" ~  species5,
                             species == "harvest.s_6" ~  species6))


basket1_rev <- all_outputs %>% 
  select(basket, per_quota, year, tot_profits, effort.t_1, starts_with("rev_per_sp")) %>% 
  pivot_longer(cols=starts_with("rev_per_sp"),
               names_to='species',
               values_to='revenue') %>% 
  mutate(species = case_when(species == "rev_per_sp.s_1" ~  species1,
                             species == "rev_per_sp.s_2" ~  species2,
                             species == "rev_per_sp.s_3" ~  species3,
                             species == "rev_per_sp.s_4" ~  species4,
                             species == "rev_per_sp.s_5" ~  species5,
                             species == "rev_per_sp.s_6" ~  species6))

basket1_harbio <- all_outputs %>% 
  select(basket, per_quota, year, tot_profits, effort.t_1, starts_with("harbio")) %>% 
  pivot_longer(cols=starts_with("harbio"),
               names_to='species',
               values_to='exploitation.rate') %>% 
  mutate(species = case_when(species == "harbio1" ~ species1,
                             species == "harbio2" ~ species2,
                             species == "harbio3" ~ species3,
                             species == "harbio4" ~ species4,
                             species == "harbio5" ~ species5,
                             species == "harbio6" ~ species6))

#basket1_harbio[is.na(basket1_harbio)] <- 0

# join
basket <-  left_join(basket1_bio, basket1_har, by=c('basket', "per_quota", "year", 
                                                     "tot_profits", "effort.t_1", "species")) %>%
  left_join(., basket1_rev, by=c('basket', "per_quota", "year", 
                                 "tot_profits", "effort.t_1", "species")) %>% 
  left_join(., basket1_harbio, by=c('basket', "per_quota", "year", 
                                    "tot_profits", "effort.t_1", "species")) %>% 
  mutate(exploitation.rate=round(exploitation.rate, 2))

# write
write.table(basket, here(fileplace, "all_results","results", "basket2.csv"),
            row.names=FALSE, sep=",")

atest <- basket %>% 
  filter(exploitation.rate>1)

atest1 <- basket %>% 
  filter(effort.t_1<0)

max_test <- max(basket$exploitation.rate, na.rm=T)

max_test1 <- max(basket$exploitation.rate, na.rm=T)