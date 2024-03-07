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
fileplace <- "basket1"
# Type of model, in this case we have the simple scenarios
fileplace1 <- "2 single_sp"


#### DATA ###########################
# Species:
# s = species, r, K, X, p
k1 <- 17.27910103	#	Coryphaena hippurus
k2 <- 822.3731754	#	Kajikia albida
k3 <- 914.4006364	#	Kajikia audax
k4 <- 935.7060958	#	Xiphias gladius
k5 <- 615.3019578	#	Thunnus albacares
k6 <- 238.3418413	#	Rachycentron canadum
k7 <- 81.62535576	#	Acanthocybium solandri
k8 <- 272.7010473	#	Seriola dumerili

p1 <- 4816.681053	#	Coryphaena hippurus
p2 <- 3019.402095	#	Kajikia albida
p3 <- 3019.402095	#	Kajikia audax
p4 <- 3019.402095	#	Xiphias gladius
p5 <- 3019.402095	#	Thunnus albacares
p6 <- 1567.746469	#	Rachycentron canadum
p7 <- 4714.274068	#	Acanthocybium solandri
p8 <- 1867.158248	#	Seriola dumerili


r1 <- 0.868098579	#	Coryphaena hippurus
r2 <- 0.288900859	#	Kajikia albida
r3 <- 0.244429707	#	Kajikia audax
r4 <- 0.234773411	#	Xiphias gladius
r5 <- 0.410515749	#	Thunnus albacares
r6 <- 0.459036753	#	Rachycentron canadum
r7 <- 0.51132494	#	Acanthocybium solandri
r8 <- 0.43938687	#	Seriola dumerili

prop=0.2

s1 <- c(s=1, r=r1, K=k1, X=prop*k1, p=p1, tcoef=0) 
s2 <- c(s=2, r=r2, K=k2, X=prop*k2, p=p2, tcoef=0) 
s3 <- c(s=3, r=r3, K=k3, X=prop*k3, p=p3, tcoef=0) 
s4 <- c(s=4, r=r4, K=k4, X=prop*k4, p=p4, tcoef=0) 
s5 <- c(s=5, r=r5, K=k5, X=prop*k5, p=p5, tcoef=0) 
s6 <- c(s=6, r=r6, K=k6, X=prop*k6, p=p6, tcoef=0) 
s7 <- c(s=7, r=r7, K=k7, X=prop*k7, p=p7, tcoef=0) 
s8 <- c(s=8, r=r8, K=k8, X=prop*k8, p=p8, tcoef=0) 

# t = tech, q1 = catchability species 1, q2 = catchability species 2...
qi1 <- 0.100254065	#	Coryphaena hippurus
qi2 <- 0.003712323	#	Kajikia albida
qi3 <- 0.003712323	#	Kajikia audax
qi4 <- 0.003712323	#	Xiphias gladius
qi5 <- 0.003712323	#	Thunnus albacares
qi6 <- 0.003712323	#	Rachycentron canadum
qi7 <- 0.214771274	#	Acanthocybium solandri
qi8 <- 0.060445281	#	Seriola dumerili

t1 <- c(t=1, q1 = qi1, q2 = 0, q3 = 0, q4 = 0, q5=0, q6=0, q7=0, q8=0)
t2 <- c(t=2, q1 = 0, q2 = qi2, q3 = 0, q4 = 0, q5=0, q6=0, q7=0, q8=0) 
t3 <- c(t=3, q1 = 0, q2 = 0, q3 = qi3, q4 = 0, q5=0, q6=0, q7=0, q8=0)
t4 <- c(t=4, q1 = 0, q2 = 0, q3 = 0, q4 = qi4, q5=0, q6=0, q7=0, q8=0)
t5 <- c(t=5, q1 = 0, q2 = 0, q3 = 0, q4 = 0, q5=qi5, q6=0, q7=0, q8=0)
t6 <- c(t=6, q1 = 0, q2 = 0, q3 = 0, q4 = 0, q5=0, q6=qi6, q7=0, q8=0)
t7 <- c(t=7, q1 = 0, q2 = 0, q3 = 0, q4 = 0, q5=0, q6=0, q7=qi7, q8=0)
t8 <- c(t=8, q1 = 0, q2 = 0, q3 = 0, q4 = 0, q5=0, q6=0, q7=0, q8=qi8)

# Tech cost: t = tech, cost = cost per unit effort
c1 <- c(t=1, cost=1)
c2 <- c(t=2, cost=1)
c3 <- c(t=3, cost=1)
c4 <- c(t=4, cost=1)
c5 <- c(t=5, cost=1)
c6 <- c(t=6, cost=1)
c7 <- c(t=7, cost=1)
c8 <- c(t=8, cost=1)

# Baskets: bimary matrix of # species x # baskets, where 1 means that species is in that basket

b1 <- c(s1 = 1, s2 = 0, s3 = 0, s4 = 0, s5=0, s6=0, s7=0, s8=0)
b2 <- c(s1 = 0, s2 = 1, s3 = 0, s4 = 0, s5=0, s6=0, s7=0, s8=0)
b3 <- c(s1 = 0, s2 = 0, s3 = 1, s4 = 0, s5=0, s6=0, s7=0, s8=0)
b4 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 1, s5=0, s6=0, s7=0, s8=0)
b5 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5=1, s6=0, s7=0, s8=0)
b6 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5=0, s6=1, s7=0, s8=0)
b7 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5=0, s6=0, s7=1, s8=0)
b8 <- c(s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5=0, s6=0, s7=0, s8=1)

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
species = list(s1, s2, s3, s4, s5, s6, s7, s8)
tech = list(t1, t2, t3, t4, t5, t6, t7, t8)
cost = list(c1, c2, c3, c4, c5, c6, c7, c8)
baskets = list(b1, b2, b3, b4, b5, b6, b7, b8)

###### OPTIMIZATION
source(here("functions", "optimal_baskets.R"))
source(here("functions", "qb_stock_m.R"))

s1h <- 0.1
s2h <- 0.1
s3h <- 0.1
s4h <- 0.1
s5h <- 0.1
s6h <- 0.1
s7h <- 0.1
s8h <- 0.1

# mortality <- c(s1h, s2h, s3h, s4h, s5h)
# mortguess1  <-  t(as.matrix(mortality))
# mortguess1= t(as.matrix(c(s1h,s2h, s3h, s4h, s5h)))
mort_guess = c(s1h, s2h, s3h, s4h, s5h, s6h, s7h, s8h)


output <- optimal_baskets(species, tech, cost, baskets, mort_guess, years)


#### SAVE RESULTS

csvname <- "npvcase.csv"
csvfile <- paste(csvname, sep="")


write.table(na.omit(output),here(fileplace1, fileplace,"results", csvfile),
            row.names=FALSE, sep=",")