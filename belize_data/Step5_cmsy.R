# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)
library(datalimited2)

# Read file
catch_year <- read_csv("step4/belize_catch_year.csv")  

# unique(catch_year$sci_name)
#[1] "Acanthocybium solandri"  "Caranx hippos"           "Centropomus undecimalis" "Coryphaena hippurus"    
#[5] "Epinephelus guttatus"    "Epinephelus itajara"     "Epinephelus striatus"    "Gerres cinereus"        
#[9] "Haemulon plumierii"      "Haemulon sciurus"        "Lachnolaimus maximus"    "Lutjanus analis"        
#[13] "Lutjanus apodus"         "Lutjanus griseus"        "Lutjanus jocu"           "Lutjanus purpureus"     
#[17] "Lutjanus synagris"       "Lutjanus vivanus"        "Mycteroperca bonaci"     "Ocyurus chrysurus"      
#[21] "Rachycentron canadum"    "Scomberomorus cavalla"   "Seriola dumerili"        "Sphyraena barracuda"    
#[25] "Thunnus albacares" 

species_list <- unique(catch_year$sci_name)

catch_df <- catch_year %>% 
  select(1, 3, 4)


#### create all df

for (i in 1:length(species_list)) {
 
df <- catch_df %>% 
    filter(sci_name==species_list[[i]])
  
assign(paste('X',i,sep=''),df)
}


# x1 "Acanthocybium solandri" 

rho=0.506271141
rho1=rho+0.01
rho2=rho-0.01

df <- X1

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]] %>% 
  mutate(species="Acanthocybium solandri" )

file_to_save <- ref_pts1

write.csv(file_to_save, file=file.path(here("step4"), "belize_1.csv"), row.names = F)

# x2 "Caranx hippos" 
rho=1.04713489
rho1=rho+0.8
rho2=rho-0.8

df <- X2

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Caranx hippos")

write.csv(file_to_save, file=file.path(here("step4"), "belize_2.csv"), row.names = F)

# x3 "Centropomus undecimalis"
rho=0.70970149
rho1=rho+0.01
rho2=rho-0.01

df <- X3

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Centropomus undecimalis" )

write.csv(file_to_save, file=file.path(here("step4"), "belize_3.csv"), row.names = F)

# x4 "Coryphaena hippurus"  
rho=0.86317902
rho1=rho+0.01
rho2=rho-0.01

df <- X4

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Coryphaena hippurus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_4.csv"), row.names = F)

# x5 "Epinephelus guttatus" #### DO THIS ##### GROUP 10        
rho=0.400428557
rho1=rho+0.01
rho2=rho-0.01

df <- X5

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Epinephelus guttatus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_5.csv"), row.names = F)

# x6  "Epinephelus itajara"  #### DO THIS ##### GROUP 9    
rho=0.15290155
rho1=rho+0.01
rho2=rho-0.01

df <- X6

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Epinephelus itajara")

write.csv(file_to_save, file=file.path(here("step4"), "belize_6.csv"), row.names = F)
 
# x7 "Epinephelus striatus"    
rho=0.317304867
rho1=rho+0.01
rho2=rho-0.01

df <- X7

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Epinephelus striatus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_7.csv"), row.names = F)

# x8 "Gerres cinereus"
rho=1.172568236
rho1=rho+0.01
rho2=rho-0.01

df <- X8

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Gerres cinereus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_8.csv"), row.names = F)

# x9 "Haemulon plumierii"   
rho=0.658933542
rho1=rho+0.01
rho2=rho-0.01

df <- X9

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Haemulon plumierii")

write.csv(file_to_save, file=file.path(here("step4"), "belize_9.csv"), row.names = F)

# x10 "Haemulon sciurus" 
rho=0.732488439
rho1=rho+0.4
rho2=rho-0.4

df <- X10

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Haemulon sciurus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_10.csv"), row.names = F)

# x11 "Lachnolaimus maximus"  
rho=0.677240017
rho1=rho+0.01
rho2=rho-0.01

df <- X11

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lachnolaimus maximus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_11.csv"), row.names = F)

# x12 "Lutjanus analis"  #### DO THIS ##### GROUP 10
rho=0.295407297
rho1=rho+0.01
rho2=rho-0.01

df <- X12

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus analis")

write.csv(file_to_save, file=file.path(here("step4"), "belize_12.csv"), row.names = F)

# x13 "Lutjanus apodus"            
rho=0.435558014
rho1=rho+0.01
rho2=rho-0.01

df <- X13

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus apodus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_13.csv"), row.names = F)

# x14 "Lutjanus griseus"        
rho=0.211600654
rho1=rho+0.01
rho2=rho-0.01

df <- X14

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus griseus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_14.csv"), row.names = F)

# x15 "Lutjanus jocu"           
rho=0.236732006
rho1=rho+0.01
rho2=rho-0.01

df <- X15

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus jocu")

write.csv(file_to_save, file=file.path(here("step4"), "belize_15.csv"), row.names = F)

# x16 "Lutjanus purpureus"  
rho=0.254154947
rho1=rho+0.1
rho2=rho-0.1

df <- X16

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus purpureus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_16.csv"), row.names = F)

# x17 "Lutjanus synagris"         
rho=0.545315911
rho1=rho+0.01
rho2=rho-0.01

df <- X17

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus synagris")

write.csv(file_to_save, file=file.path(here("step4"), "belize_17.csv"), row.names = F)

# x18 "Lutjanus vivanus"      
rho=0.223868246
rho1=rho+0.01
rho2=rho-0.01

df <- X18

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Lutjanus vivanus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_18.csv"), row.names = F)

# x19 "Mycteroperca bonaci" #### DO THIS ##### GROUP 9        
rho=0.199411942
rho1=rho+0.01
rho2=rho-0.01

df <- X19

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Mycteroperca bonaci")

write.csv(file_to_save, file=file.path(here("step4"), "belize_19.csv"), row.names = F)

# x20 "Ocyurus chrysurus"
rho=0.51584057
rho1=rho+0.01
rho2=rho-0.01

df <- X20

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Ocyurus chrysurus")

write.csv(file_to_save, file=file.path(here("step4"), "belize_20.csv"), row.names = F)

# x21 "Rachycentron canadum"     
rho=0.454214681
rho1=rho+0.01
rho2=rho-0.01

df <- X21

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Rachycentron canadum")

write.csv(file_to_save, file=file.path(here("step4"), "belize_21.csv"), row.names = F)

# x22 "Scomberomorus cavalla"   
rho=0.156483417
rho1=rho+0.4
rho2=rho-0.05

df <- X22

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Scomberomorus cavalla")

write.csv(file_to_save, file=file.path(here("step4"), "belize_22.csv"), row.names = F)

# x23 "Seriola dumerili"        
rho=0.43441977
rho1=rho+0.01
rho2=rho-0.01

df <- X23

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Seriola dumerili")

write.csv(file_to_save, file=file.path(here("step4"), "belize_23.csv"), row.names = F)

# x24 "Sphyraena barracuda" 
rho=0.167066229
rho1=rho+0.01
rho2=rho-0.01

df <- X24

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Sphyraena barracuda")

write.csv(file_to_save, file=file.path(here("step4"), "belize_24.csv"), row.names = F)

# x25 "Thunnus albacares" 
rho=0.410515749
rho1=rho+0.01
rho2=rho-0.01

df <- X25

output1 <- datalimited2::cmsy2(year=df$year, catch=df$catch_mt, r.low=rho2, r.hi=rho1)

ref_pts1 <- output1[["ref_pts"]]

file_to_save <- ref_pts1%>% 
  mutate(species="Thunnus albacares")

write.csv(file_to_save, file=file.path(here("belize_data", "step5"), "belize_25.csv"), row.names = F)
