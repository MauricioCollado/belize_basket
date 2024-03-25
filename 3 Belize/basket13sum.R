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
fileplace <- "3 Belize"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "basket13"

########################################################################
########################################################################
# input parameters

# Technology array

qi1 <- 0.09983336	#	Ocyurus chrysurus
qi2 <- 0.510417358	#	Lutjanus synagris

qi7 <- 0 #complete other with zero

# 0.0001

t1 <- c(t=1, q1 = qi1, q2 = qi2) # one technology is good
t2 <- c(t=2, q1 = qi7, q2 = qi7) # the other technology is useless

# Tech cost: t = tech, cost = cost per unit effort

c1 <- c(t=1, cost=1)
c2 <- c(t=2, cost=1)


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

k1 <- 5457.931549	#	Ocyurus chrysurus
k2 <- 2547.863352	#	Lutjanus synagris

p1 <- 2811.298144	#	Ocyurus chrysurus
p2 <- 2082.965728	#	Lutjanus synagris

r1 <- 0.521393627	#	Ocyurus chrysurus
r2 <- 0.550254159	#	Lutjanus synagris

prop1= 0.4	#	Ocyurus chrysurus
prop2= 0.6	#	Lutjanus synagris

s1 <- c(s=1, r=r1, K=k1, X=k1*prop1, p=p1, tcoef=0) #	Ocyurus chrysurus
s2 <- c(s=2, r=r2, K=k2, X=k2*prop2, p=p2, tcoef=0) #	Lutjanus synagris

########################################################################
########################################################################
# quota parameters

# basket bundles 1
b1 <- c(s1 = 1, s2 = 1)

# msy info
msy_1 <- 711.4326819	#	Ocyurus chrysurus
msy_2 <- 350.4931015	#	Lutjanus synagris

mortguess1_1 <- msy_1 + msy_2

mortguess1_1 <- rep(mortguess1_1 , each = 30)

mortguess1 <- c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 24.23, 385.96, 561.46, 612, 
                612, 612, 612, 612, 612, 612, 612, 612, 612, 612,612, 612, 612, 612, 612, 
                612, 612, 612, 612, 612, 612)
                

multiplier <- seq(0.01, 1, by = 0.01) 

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

source(here("functions_cc_fixed", "optimal_baskets_cc_op2.R"))
source(here("functions_cc_fixed", "qb_stock_m_cc_op2.R"))

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