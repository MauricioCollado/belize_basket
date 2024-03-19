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
fileplace <- "basket3"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "2 single_sp"


#### DATA ###########################
# Species:
# s = species, r, K, X, p
k1 <- 321.9084813	#	Lutjanus apodus
k2 <- 810.2637926	#	Lutjanus mahogoni
k3 <- 316.1966481	#	Haemulon parra
k4 <- 417.9947039	#	Haemulon album

p1 <- 1867.158248
p2 <- 1867.158248
p3 <- 1867.158248
p4 <- 1867.158248

r1 <- 0.440541776	#	Lutjanus apodus
r2 <- 0.295120103	#	Lutjanus mahogoni
r3 <- 0.689627201	#	Haemulon parra
r4 <- 0.572614232	#	Haemulon album

prop=0.6
prop1=0.2

s1 <- c(s=1, r=r1, K=k1, X=prop*k1, p=p1, tcoef=0) 
s2 <- c(s=2, r=r2, K=k2, X=prop1*k2, p=p2, tcoef=0) #	Lutjanus mahogoni
s3 <- c(s=3, r=r3, K=k3, X=prop*k3, p=p3, tcoef=0) 
s4 <- c(s=4, r=r4, K=k4, X=prop*k4, p=p4, tcoef=0) 


# t = tech, q1 = catchability species 1, q2 = catchability species 2...
qi1 <- 0.0708011	#	Lutjanus apodus
qi2 <- 0.017700275 #	Lutjanus mahogoni
qi3 <- 0.017700275	#	Haemulon parra
qi4 <- 0.017700275	#	Haemulon album
#qi5 <- 0.000874
#qi6 <- 0.000874


qi7 <- 0

t1 <- c(t=1, q1 = qi1, q2 = 0, q3 = 0, q4 = 0) # one technology is good
t2 <- c(t=2, q1 = 0, q2 = qi2, q3 = 0, q4 = 0) # the other technology is useless
t3 <- c(t=3, q1 = 0, q2 = 0, q3 = qi3, q4 = 0) # one technology is good
t4 <- c(t=4, q1 = 0, q2 = 0, q3 = 0, q4 = qi4) # the other technology is useless

# Tech cost: t = tech, cost = cost per unit effort
c1 <- c(t=1, cost=1)
c2 <- c(t=2, cost=1)
c3 <- c(t=3, cost=1)
c4 <- c(t=4, cost=1)

# Baskets: bimary matrix of # species x # baskets, where 1 means that species is in that basket

b1 <- c(s1 = 1, s2 = 0, s3 = 0, s4 = 0)
b2 <- c(s1 = 0, s2 = 1, s3 = 0, s4 = 0)
b3 <- c(s1 = 0, s2 = 0, s3 = 1, s4 = 0)
b4 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 1)


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
species = list(s1, s2, s3, s4)
tech = list(t1, t2, t3, t4)
cost = list(c1, c2, c3, c4)
baskets = list(b1, b2, b3, b4)

###### OPTIMIZATION
source(here("functions", "optimal_baskets.R"))
source(here("functions", "qb_stock_m.R"))

s1h <- 0.1
s2h <- 0.1
s3h <- 0.1
s4h <- 0.1


# mortality <- c(s1h, s2h, s3h, s4h, s5h)
# mortguess1  <-  t(as.matrix(mortality))
# mortguess1= t(as.matrix(c(s1h,s2h, s3h, s4h, s5h)))
mort_guess = c(s1h, s2h, s3h, s4h)


output <- optimal_baskets(species, tech, cost, baskets, mort_guess, years)


#### SAVE RESULTS

csvname <- "npvcase.csv"
csvfile <- paste(csvname, sep="")


write.table(na.omit(output),here(fileplace1, fileplace,"results", csvfile),
            row.names=FALSE, sep=",")