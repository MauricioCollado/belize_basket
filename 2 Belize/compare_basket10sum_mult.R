###### Different progress for each limit
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

# Where to save datasets
fileplace <- "2 Belize"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "basket10sum_mult"
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
# optimal path

mortguess1 <- c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 24.23, 385.96, 561.46, 612, 
                612, 612, 612, 612, 612, 612, 612, 612, 612, 612,612, 612, 612, 612, 612, 
                612, 612, 612, 612, 612, 612)

year_mort <- seq(1,30, 1)

optim_path <- data.frame(year_mort, mortguess1) 

names(optim_path) <- c("year", "harvest.s_1")


########################################################################
########################################################################
# extra data
# MSY inputs

# additional data. 
# NOTE: this applies if every species hjave the same K


k1 <- 8156.8715 #analis
k2 <- 163.63 #guttatus
BMSY1=0.5*k1
BMSY2=0.5*k2

#MSY6=0.5*k6

msy_thresh=0.99999
years=30

########################################################################
########################################################################

# rep(c(1, 2, 3), times=10)
# seq(0,30)

output_base <- df_col %>% 
  mutate(year = rep(seq(0,30), times=20),
         bmsy1=stock.s_1/BMSY1,
         bmsy2=stock.s_2/BMSY2,
         #msy3=stock.s_3/MSY3,
         #msy4=stock.s_4/MSY4,
         #msy5=stock.s_5/MSY5,
         #msy6=stock.s_6/MSY6,
         bmsy1p=0,
         bmsy2p=0,
         #bmsy3p=0
         #msy4p=0,
         #msy5p=0,
         #msy6p=0
  )


#Group them
all_outputs <- output_base  %>% 
  mutate(tot_profits = profit_per_t.t_1 + profit_per_t.t_2, 
         tot_stock = stock.s_1 + stock.s_2,
         cpue1_1 = harvest.s_1/effort.t_1,
         cpue2_1 = harvest.s_2/effort.t_1,
         gcpue1_1=(cpue1_1 - lag(cpue1_1))/lag(cpue1_1),
         gcpue2_1=(cpue2_1 - lag(cpue2_1))/lag(cpue2_1)) %>% 
  mutate(id2 = case_when(id == "005" ~ "5",
                         id == "01" ~ "10",
                         id == "015" ~ "15",
                         id == "02" ~ "20",
                         id == "025" ~ "25",
                         id == "03" ~ "30",
                         id == "035" ~ "35",
                         id == "04" ~ "40",
                         id == "045" ~ "45",
                         id == "05" ~ "50",
                         id == "055" ~ "55",
                         id == "06" ~ "60",
                         id == "065" ~ "65",
                         id == "07" ~ "70",
                         id == "075" ~ "75",
                         id == "08" ~ "80",
                         id == "085" ~ "85",
                         id == "09" ~ "90",
                         id == "095" ~ "95",
                         id == "1" ~ "100"),
                         #id == "11" ~ "110",
                         #id == "12" ~ "120",
                         #id == "13" ~ "130",
                         #id == "14" ~ "140",
                         #id == "15" ~ "150"),
         id2=ifelse(is.na(id2), id, id2),
         id = id2) %>% 
  select(-id2) %>% 
  mutate(Label = ifelse(year == 30, id, NA),
         Label1 = ifelse(year == 1, id, NA))


# create msy indicator function
#create indicator of reaching MSY

all_outputs$bmsy1p<-ifelse(all_outputs$bmsy1>msy_thresh, 1, 0)
all_outputs$bmsy2p<-ifelse(all_outputs$bmsy2>msy_thresh, 1, 0)
#all_outputs$msy3p<-ifelse(all_outputs$msy3>msy_thresh, 1, all_outputs$msy3p)
#all_outputs$msy4p<-ifelse(all_outputs$msy4>msy_thresh, 1, all_outputs$msy4p)
#all_outputs$msy5p<-ifelse(all_outputs$msy5>msy_thresh, 1, all_outputs$msy5p)
#all_outputs$msy6p<-ifelse(all_outputs$msy6>msy_thresh, 1, all_outputs$msy6p)

########################################################################
########################################################################
# GRAPHS
########################################################################
########################################################################
### STOCK

title1 <- "Stock Mutton Snapper (Lutjanus analis) "
title2 <- "Stock Red Jind (Epinephelus guttatus)"

subtitle1 <- "1 basket, 2 species, 1 gear type, MSY sum quota"

mytheme<-theme( strip.background = element_rect(fill="white"),
                axis.ticks.length = unit(-0.05, "in"),
                axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.text.x = element_markdown(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
                legend.position = "bottom")

abio1 <- ggplot(data = all_outputs, aes(x=year, y=stock.s_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 1)+ 
  labs(title=title1,
       subtitle=subtitle1,
       y= "Stock",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")+
  geom_hline(yintercept=c(0.25*k1, 0.5*k1), linetype='dashed') +
  annotate(geom="text", x=0, y=0.3*k1, label="25% K") +
  annotate(geom="text", x=0, y=0.55*k1, label="50% K")

abio1

abio2 <- ggplot(data = all_outputs, aes(x=year, y=stock.s_2, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 1) + 
  labs(title=title2,
       subtitle=subtitle1,
       y= "Stock",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme+
  theme(legend.position = "none")+
  geom_hline(yintercept=c(0.25*k2, 0.5*k2), linetype='dashed') +
  annotate(geom="text", x=0, y=0.3*k2, label="25% K") +
  annotate(geom="text", x=0, y=0.55*k2, label="50% K")

#  geom_hline(yintercept=c(0.25*k2, 0.5*k2), linetype='dashed', color=c('blue', 'red'))
abio2

#save files
msylist <- c("abio1", "abio2")

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1, "figures", file=paste0("abio", y ,".png")), height = 5, width = 8)
}

########################################################################
########################################################################
### REVENUE

all_outputsr <- all_outputs  %>% 
  mutate(Label = ifelse(year == 20, id, NA))

title1rev <- "Revenue (30y) Mutton Snapper (Lutjanus analis)"
title2rev <- "Revenue (30y) Red Jind (Epinephelus guttatus)"

rev1 <- ggplot(data = all_outputsr, aes(x=year, y=rev_per_sp.s_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label1), nudge_x = 0.35, size = 1)+ 
  labs(title=title1rev,
       subtitle=subtitle1,
       y= "Revenue $",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

rev1 

rev2 <- ggplot(data = all_outputsr, aes(x=year, y=rev_per_sp.s_2, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label1), nudge_x = 0.35, size = 1)+ 
  labs(title=title2rev,
       subtitle=subtitle1,
       y= "Revenue $",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

rev2 

# save graphs

msylist <- c("rev1", "rev2")
#msylist <- as.list(msylist)

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1,"figures", file=paste0("rev", y ,".png")), height = 5, width = 8)
}

########################################################################
########################################################################
### HARVEST

title1har <- "Harvest (30y) Mutton Snapper (Lutjanus analis)"
title2har <- "Harvest (30y) Red Jind (Epinephelus guttatus)"

har1 <- ggplot()+
  geom_line(data = all_outputsr, aes(x=year, y=harvest.s_1, color=id))+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  ### geom_point(data=optim_path, aes(x=year, y=harvest.s_1), size=1) +
  geom_label(data = all_outputsr, aes(x=year, y=harvest.s_1, label = Label1, alpha=0.1), nudge_x = 0.35, size = 1)+ 
  labs(title=title1har,
       subtitle=subtitle1,
       y= "Stock",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

har1 

har2 <- ggplot(data = all_outputsr, aes(x=year, y=harvest.s_2, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label1), nudge_x = 0.35, size = 1)+ 
  labs(title=title2har,
       subtitle=subtitle1,
       y= "Harvest",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

har2 

# save graphs
msylist <- c("har1", "har2")
#msylist <- as.list(msylist)

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1,"figures", file=paste0("har", y ,".png")), height = 5, width = 8)
}

########################################################################
########################################################################
### CPUE

title1ef <- "Effort (30y) on generic technology"
title2ef <- "Effort (30y) on NONE technology"

eff1 <- ggplot(data = all_outputs, aes(x=year, y=effort.t_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label1), nudge_x = 0.35, size = 1)+ 
  labs(title=title1ef,
       subtitle=subtitle1,
       y= "Effort",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

eff1 

eff2 <- ggplot(data = all_outputs, aes(x=year, y=effort.t_2, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label1), nudge_x = 0.35, size = 1)+ 
  labs(title=title2ef,
       subtitle=subtitle1,
       y= "Effort",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

eff2 

# save graphs
msylist <- c("eff1", "eff2")
#msylist <- as.list(msylist)

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1,"figures", file=paste0("effort", y ,".png")), height = 5, width = 8)
}

########################################################################
########################################################################
### catch per unit of effort

title1cpue <- "CPUE (30y) of gillnets on Mutton Snapper (Lutjanus analis)"
title2cpue <- "CPUE (30y) of gillnets on Red Jind (Epinephelus guttatus)"

cpue1_1 <- ggplot(data = all_outputs, aes(x=year, y=cpue1_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 2)+ 
  labs(title=title1cpue,
       subtitle=subtitle1,
       y= "CPUE",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

cpue1_1

cpue1_2 <- ggplot(data = all_outputs, aes(x=year, y=cpue2_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 2)+ 
  labs(title=title2cpue,
       subtitle=subtitle1,
       y= "CPUE",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

cpue1_2

# save graphs
msylist <- c("cpue1_1", "cpue1_2")
#msylist <- as.list(msylist)

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1,"figures", file=paste0("cpue", y ,".png")), height = 5, width = 8)
}

########################################################################
########################################################################
### biomass indicator

bio_all <- all_outputs %>% 
  select(-Label, -Label1)
  
# bio_all <- drop_na(bio_all)

bio_all <- bio_all %>%
  group_by(id) %>%
  select(starts_with("bmsy")) %>% 
  summarise(s1=sum((bmsy1p)/years),
            s2=sum((bmsy2p)/years),
            .groups = 'drop'
  ) %>% 
  ungroup()

write.table(na.omit(bio_all), here(fileplace, fileplace1,"tables", "lut_gut.csv"),
            row.names=FALSE, sep=",")

########################################################################
########################################################################
### cpue growth

title1cpue <- "Growth CPUE (30y) of gillnets on Mutton Snapper (Lutjanus analis)"
title2cpue <- "Growth CPUE (30y) of gillnets on Red Jind (Epinephelus guttatus)"

gcpue1_1 <- ggplot(data = all_outputs, aes(x=year, y=gcpue1_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 2)+ 
  labs(title=title1cpue,
       subtitle=subtitle1,
       y= "Growth CPUE",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

gcpue1_1

gcpue1_2 <- ggplot(data = all_outputs, aes(x=year, y=gcpue2_1, color=id))+
  geom_line()+
  #geom_line(aes(y=stock.s_2), color = "blue", size=1)+
  #geom_line(aes(y=stock.s_3), color = "green", size=1)+
  #geom_line(aes(y=stock.s_4), color = "orange", size=1)+
  #geom_line(aes(y=stock.s_5), color = "purple", size=1)+
  #geom_line(aes(y=stock.s_6), color = "black", size=1)+
  #geom_line(aes(y=stock.s_3), color = "black", size=1)+
  geom_label(aes(label = Label), nudge_x = 0.35, size = 2)+ 
  labs(title=title2cpue,
       subtitle=subtitle1,
       y= "Growth CPUE",
       x= "Year")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme +
  theme(legend.position = "none")

gcpue1_2

# save graphs
msylist <- c("gcpue1_1", "gcpue1_2")
#msylist <- as.list(msylist)

for(y in 1:length(msylist)){ 
  #filename = paste("msy_", y,".png", sep="")
  # plot1 <- msylist[[y]]
  ggsave(plot = get(msylist[y]), filename = here(fileplace, fileplace1,"figures", file=paste0("gcpue", y ,".png")), height = 5, width = 8)
}

######## revenue

npv <-  read_csv(here("2 single_sp", "basket10", "results", "npvcase.csv"))

single <- npv[ , grepl( "rev_per" , names(npv))] %>% 
  mutate(year = seq(0,30))

single <- drop_na(single)

rev_single <- single %>%
  summarise(s1=sum((rev_per_sp.s_1)),
            s2=sum((rev_per_sp.s_2)),
            #s3=sum((rev_per_sp.s_3)),
            #s4=sum((rev_per_sp.s_4)),
            #s5=sum((rev_per_sp.s_5)),
            #s6=sum((rev_per_sp.s_6))
  ) 

all_rev <- all_outputs

all_rev  <- all_rev %>% 
  select(id, year, starts_with("rev_per"))

all_rev  <- drop_na(all_rev)

all_rev1 <- all_rev  %>%
  group_by(id) %>%
  summarise(s1=sum((rev_per_sp.s_1)),
            s2=sum((rev_per_sp.s_2)),
            #s3=sum((rev_per_sp.s_3.x)),
            #s4=sum((rev_per_sp.s_4.x)),
            #s5=sum((rev_per_sp.s_5.x)),
            #s6=sum((rev_per_sp.s_6.x)),
            .groups = 'drop'
  )  



all_rev1$s1 <- all_rev1$s1/rev_single$s1
all_rev1$s2 <- all_rev1$s2/rev_single$s2
#all_rev1$s3 <- all_rev1$s3/rev_single$s3
#all_rev1$s4 <- all_rev1$s4/rev_single$s4
#all_rev1$s5 <- all_rev1$s5/rev_single$s5
#all_rev1$s6 <- all_rev1$s6/rev_single$s6


######## profit

profit_single <- npv %>% 
  select(starts_with("profit_per")) %>% 
  mutate(year = seq(0,30))

profit_single <- drop_na(profit_single)

pro <-profit_single %>%
  summarise(s1=sum((profit_per_t.t_1)),
            s2=sum((profit_per_t.t_2))
            #s3=sum((rev_per_sp.s_3)),
            #s4=sum((rev_per_sp.s_4)),
            #s5=sum((rev_per_sp.s_5)),
            #s6=sum((rev_per_sp.s_6))
  ) %>% 
  mutate(total = s1 + s2)

all_profits  <- all_outputs %>% 
  select(id, year, starts_with("profit_per"))


all_profits <- drop_na(all_profits  )

all_profits1  <- all_profits    %>%
  group_by(id) %>%
  summarise(s1=sum((profit_per_t.t_1)),
            s2=sum((profit_per_t.t_2)),
            #s3=sum((rev_per_sp.s_3.x)),
            #s4=sum((rev_per_sp.s_4.x)),
            #s5=sum((rev_per_sp.s_5.x)),
            #s6=sum((rev_per_sp.s_6.x)),
            .groups = 'drop'
  )  %>% 
  mutate(total = s1 + s2)


all_profits1$total <- all_profits1$total/pro$total

all_profits1 <- all_profits1 %>% 
  select(id, total)

write.table(na.omit(all_profits1), here(fileplace, fileplace1,"tables", "profits.csv"),
            row.names=FALSE, sep=",")


###### join
success <- all_rev <- left_join(bio_all, all_profits1, by="id") %>% 
  mutate(order = as.numeric(as.character(id))) 

write.table(na.omit(success), here(fileplace, fileplace1,"tables", "combined_result.csv"),
            row.names=FALSE, sep=",")


success_long <- success %>% 
  pivot_longer(
    cols = starts_with("s"),
    names_to = "species",
    names_prefix = "s",
    values_to = "bio",
    values_drop_na = TRUE
  ) %>% 
  mutate(species = case_when(species == 1 ~ "Mutton Snapper",
                             species == 2 ~ "Red hind"))

success_title <- "Proportion of MSY-sum quota and success rate"

final_result <- ggplot(success_long, aes(x = bio, y = total, label=order)) +
  geom_point(aes(color=species))+
  geom_text(hjust=1.5, vjust=0, size = 6/.pt)+
  labs(title=success_title,
       subtitle=subtitle1,
       y= "Profit ratio",
       x= "% of succcesful conservation years")+
  expand_limits(y = 0) +
  theme_bw(base_size = 12) +
  mytheme

final_result

ggsave(plot = final_result, filename = here(fileplace, fileplace1,"figures", "final_graph.png"), height = 5, width = 8)
