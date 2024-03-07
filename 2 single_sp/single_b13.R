###### Single species
###### Code by Mauricio Collado

###### Compare progress for each harvest SUM limit
#####

# erase
rm(list = ls(all = TRUE)) 

#packages
library(tidyverse)
library(ggplot2)
library(here)
library(quadprog)
library(future)
library(jsonlite)
library(purrr)
library(data.table)

# Where to save datasets
fileplace <- "basket13"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "2 single_sp"


#### DATA ###########################
# Species:
# s = species, r, K, X, p
k1 <- 5457.931549	#	Ocyurus chrysurus
k2 <- 2547.863352	#	Lutjanus synagris
#k3 <- 522.7108
#k4 <- 125.8112

p1 <- 2811.298144	#	Ocyurus chrysurus
p2 <- 2082.965728	#	Lutjanus synagris
#p3 <- 1749.82
#p4 <- 2398.5

r1 <- 0.521393627	#	Ocyurus chrysurus
r2 <- 0.550254159	#	Lutjanus synagris

prop1= 0.4	#	Ocyurus chrysurus
prop2= 0.6	#	Lutjanus synagris

s1 <- c(s=1, r=r1, K=k1, X=k1*prop1, p=p1, tcoef=0) #	Ocyurus chrysurus
s2 <- c(s=2, r=r2, K=k2, X=k2*prop2, p=p2, tcoef=0) #	Lutjanus synagris


# t = tech, q1 = catchability species 1, q2 = catchability species 2...
qi1 <- 0.09983336	#	Ocyurus chrysurus
qi2 <- 0.510417358	#	Lutjanus synagris
#qi4 <- 0.000874
#qi5 <- 0.000874
#qi6 <- 0.000874

qi7 <- 0

t1 <- c(t=1, q1 = qi1, q2 = qi7)
t2 <- c(t=2, q1 = qi7, q2 = qi2)

# Tech cost: t = tech, cost = cost per unit effort
c1 <- c(t=1, cost=1)
c2 <- c(t=2, cost=1)

# Baskets: bimary matrix of # species x # baskets, where 1 means that species is in that basket

b1 <- c(s1 = 1, s2 = 0)
b2 <- c(s1 = 0, s2 = 1)

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

# without normalization


##### INPUTS

# Change these to reflect the number of species and technologies above:
species = list(s1, s2)
tech = list(t1, t2)
cost = list(c1, c2)
baskets = list(b1, b2)

###### OPTIMIZATION
source(here("functions", "optimal_baskets.R"))
source(here("functions", "qb_stock_m.R"))

s1h <- 0.1
s2h <- 0.1


# mortality <- c(s1h, s2h, s3h, s4h, s5h)
# mortguess1  <-  t(as.matrix(mortality))
# mortguess1= t(as.matrix(c(s1h,s2h, s3h, s4h, s5h)))
mort_guess = c(s1h, s2h)


output <- optimal_baskets(species, tech, cost, baskets, mort_guess, years)


#### SAVE RESULTS

csvname <- "npvcase.csv"
csvfile <- paste(csvname, sep="")


write.table(na.omit(output),here(fileplace1, fileplace,"results", csvfile),
            row.names=FALSE, sep=",")