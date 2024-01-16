###### Different progress for each limit
###### Code by Mauricio Collado

###### Different progress for each limit, start at max K

# erase
rm(list = ls(all = TRUE)) 


# packages
library(tidyverse)
library(here)
library(quadprog)
library(future)

# Where to save datasets
fileplace <- "1 Belize"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "case9"

########################################################################
########################################################################
# input parameters

# Technology array

qi1 <- 0.0008
qi2 <- 0.001
#qi3 <- 0.0006
#qi4 <- 0.000874
#qi5 <- 0.000874
#qi6 <- 0.000874

qi7 <- 0 #complete other with zero

# 0.0001

t1 <- c(t=1, q1 = qi1, q2 = qi2) # one technology is good
t2 <- c(t=2, q1 = qi7, q2 = qi7) # the other technology is useless

# Tech cost: t = tech, cost = cost per unit effort

c1 <- c(t=1, cost=1)
c2 <- c(t=2, cost=1)
#c3 <- c(t=3, cost=1)
#c4 <- c(t=4, cost=1)
#c5 <- c(t=5, cost=1)
#c6 <- c(t=6, cost=1)

# Years
years = 35

# define base, max and min temp
temp_min=19
temp_max=26
# temp = runif(years, min = temp_min, max = temp_max) 

temp = c(24.96186, 22.78344, 23.18393, 21.37994, 21.63414, 22.50235, 21.18345, 25.96468, 
         20.91578, 19.54157, 23.54480, 24.30295, 20.24743, 25.40967, 20.77083, 22.85526, 
         21.43720, 25.29788, 19.55776, 23.66476, 24.93708, 23.00850, 24.87281, 23.02251, 
         23.28070, 24.51235, 19.96620, 20.82423, 25.95001, 24.94559)

temp_base=mean(unlist(temp))

# define SST anomaly
temp_anom=temp-temp_base

########################################################################
########################################################################
# species parameters

k1 <- 8156.8715 #analis
k2 <- 163.63 #guttatus
#k3 <- 522.7108
#k4 <- 125.8112

p1 <- 2590.2
p2 <- 1914.21
#p3 <- 1749.82
#p4 <- 2398.5


s1 <- c(s=1, r=0.3, K=k1, X=k1, p=p1, tcoef=0) #analis
s2 <- c(s=2, r=0.4045, K=k2, X=k2, p=p2, tcoef=0) #guttatus

########################################################################
########################################################################
# quota parameters

# basket bundles 1
b1 <- c(s1 = 1, s2 = 1)

# msy info
msy_1 <- 612.28	#analis
msy_2 <- 16.55 #guttatus

mortguess1_1 <- msy_1 + msy_2

mortguess1_1 <- rep(mortguess1_1 , each = 50)

mortguess1 <- c(1547.882353,
                1227.714551,
                972.4330969,
                809.6633105,
                714.321799,
                660.6165096,
                630.9490618,
                614.7253823,
                605.9007877,
                601.1145019,
                598.5224985,
                597.1199652,
                596.361393,
                595.9512116,
                595.7294438,
                595.6095518,
                595.5447383,
                595.5097008,
                595.4907602,
                595.4805213,
                595.4749864,
                595.4719944,
                595.470377,
                595.4695026,
                595.46903,
                595.4687745,
                595.4686364,
                595.4685617,
                595.4685213,
                595.4684995,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595,
                595)
                

multiplier <- seq(0.05, 1, by = 0.05) 

########################################################################
########################################################################
# input list

species = list(s1, s2)
tech = list(t1, t2)
cost = list(c1, c2)
baskets = list(b1)

########################################################################
########################################################################
# input list
# load function

source(here("functions_cc_fixed", "optimal_baskets_cc_op.R"))
source(here("functions_cc_fixed", "qb_stock_m_cc_op.R"))

# loop across each scenario
for (i in multiplier){
  mortguess2 <- mortguess1_1*i
  
  output <- optimal_baskets(species, tech, cost, baskets, mort_guess=mortguess2, years)
  
  csvname <- paste0("output_", i,"_.csv")
  csvfile <- paste(csvname, sep="")
  write.table(na.omit(output),here(fileplace, fileplace1,"results", csvfile),
              row.names=FALSE, sep=",")
}

########################################################################
########################################################################