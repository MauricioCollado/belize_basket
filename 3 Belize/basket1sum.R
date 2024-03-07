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
fileplace1 <- "basket1"

########################################################################
########################################################################
# input parameters

# Technology array

qi1 <- 0.100254065	#	Coryphaena hippurus
qi2 <- 0.003712323	#	Kajikia albida
qi3 <- 0.003712323	#	Kajikia audax
qi4 <- 0.003712323	#	Xiphias gladius
qi5 <- 0.003712323	#	Thunnus albacares
qi6 <- 0.003712323	#	Rachycentron canadum
qi7 <- 0.214771274	#	Acanthocybium solandri
qi8 <- 0.060445281	#	Seriola dumerili


# 0.0001

t1 <- c(t=1, q1 = qi1, q2 = qi2, q3 = qi3, q4 = qi4, q5=qi5, q6=qi6, q7=qi7, q8=qi8) # one technology is good
t2 <- c(t=2, q1 = 0, q2 = 0, q3 = 0, q4 = 0, q5=0, q6=0, q7=0, q8=0) # the other technology is useless

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

########################################################################
########################################################################
# quota parameters

# basket bundles 1
b1 <- c(s1 = 1, s2 = 1, s3 = 1, s4 = 1, s5 = 1, s6 = 1, s7 = 1, s8 = 1)

# msy info
msy_1 <- 3.749990764	#	Coryphaena hippurus
msy_2 <- 59.3960792	#	Kajikia albida
msy_3 <- 55.87666991	#	Kajikia audax
msy_4 <- 54.91972795	#	Xiphias gladius
msy_5 <- 63.14778602	#	Thunnus albacares
msy_6 <- 27.35191624	#	Rachycentron canadum
msy_7 <- 10.43427004	#	Acanthocybium solandri
msy_8 <- 29.95531494	#	Seriola dumerili

mortguess1_1 <- msy_1 + msy_2 + msy_3 + msy_4 + msy_5 + msy_6 + msy_7 + msy_8

mortguess1_1 <- rep(mortguess1_1 , each = 30)

mortguess1 <- c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 24.23, 385.96, 561.46, 612, 
                612, 612, 612, 612, 612, 612, 612, 612, 612, 612,612, 612, 612, 612, 612, 
                612, 612, 612, 612, 612, 612)
                

#multiplier <- seq(0.05, 1, by = 0.05) 
multiplier <- seq(0.01, 1, by = 0.01) 

########################################################################
########################################################################
# input list

species = list(s1, s2, s3, s4, s5, s6, s7, s8)
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