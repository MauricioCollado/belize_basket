###### Different progress for each limit
###### Code by Mauricio Collado

###### Different progress for each limit

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
fileplace1 <- "case5"

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
years = 30

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


s1 <- c(s=1, r=0.3, K=k1, X=k1*0.1, p=p1, tcoef=0) #analis
s2 <- c(s=2, r=0.4045, K=k2, X=k2*0.1, p=p2, tcoef=0) #guttatus

########################################################################
########################################################################
# quota parameters

# basket bundles 1
b1 <- c(s1 = 1, s2 = 1)


#mortguess1 <- c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 24.23, 385.96, 561.46, 612, 
#                612, 612, 612, 612, 612, 612, 612, 612, 612, 612,612, 612, 612, 612, 612, 
#                612, 612, 612, 612, 612, 612)

mortguess1 <- c(0.00001,
                0.00001,
                0.00001,
                0.00001,
                0.00001,
                0.00001,
                0.00001,
                63.52840626,
                321.7904004,
                451.5504848,
                518.8416954,
                554.3876054,
                573.3609044,
                583.5467967,
                589.0324205,
                591.9917899,
                593.5897917,
                594.4531175,
                594.9196596,
                595.1718167,
                595.3081137,
                595.3817887,
                595.4216145,
                595.443143,
                595.4547807,
                595.4610717,
                595.4644724,
                595.4663108,
                595.4673045,
                595.4678418)



multiplier <- seq(0.05, 1.5, by = 0.05) 

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
  mortguess2 <- mortguess1*i
  
  output <- optimal_baskets(species, tech, cost, baskets, mort_guess=mortguess2, years)
  
  csvname <- paste0("output_", i,"_.csv")
  csvfile <- paste(csvname, sep="")
  write.table(na.omit(output),here(fileplace, fileplace1,"results", csvfile),
              row.names=FALSE, sep=",")
}

########################################################################
########################################################################