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
fileplace1 <- "basket7"

########################################################################
########################################################################
# input parameters

# Technology array
qi1 <- 0.05748786	#	Centropomus undecimalis
qi2 <- 0.014371965	#	Petenia splendida
qi3 <- 0.014371965	#	Ictalurus furcatus

qi7 <- 0 #complete other with zero

# 0.0001

t1 <- c(t=1, q1 = qi1, q2 = qi2, q3 = qi3) # one technology is good
t2 <- c(t=2, q1 = qi7, q2 = qi7, q3 = qi7) # the other technology is useless

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

k1 <- 205.9351051	#	Centropomus undecimalis
k2 <- 243.0740296	#	Petenia splendida
k3 <- 577.0963702	#	Ictalurus furcatus

p1 <- 2419.837831	#	Centropomus undecimalis
p2 <- 2419.837831	
p3 <- 2419.837831	
#p4 <- 2398.5

r1 <- 0.714712439	#	Centropomus undecimalis
r2 <- 0.799887753	#	Petenia splendida
r3 <- 0.437390934	#	Ictalurus furcatus


prop=0.2

s1 <- c(s=1, r=r1, K=k1, X=k1*prop, p=p1, tcoef=0) #	Centropomus undecimalis
s2 <- c(s=2, r=r2, K=k2, X=k2*prop, p=p2, tcoef=0) #	Petenia splendida
s3 <- c(s=3, r=r3, K=k3, X=k2*prop, p=p3, tcoef=0) #	Ictalurus furcatus

########################################################################
########################################################################
# quota parameters

# basket bundles 1
b1 <- c(s1 = 1, s2 = 1, s3 = 1)

# msy info
msy_1 <- 36.79609532	#	Centropomus undecimalis
msy_2 <- 48.60798483	#	Petenia splendida
msy_3 <- 63.10418009	#	Ictalurus furcatus

mortguess1_1 <- msy_1 + msy_2 + msy_3 

mortguess1_1 <- rep(mortguess1_1 , each = 30)

mortguess1 <- c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 24.23, 385.96, 561.46, 612, 
                612, 612, 612, 612, 612, 612, 612, 612, 612, 612,612, 612, 612, 612, 612, 
                612, 612, 612, 612, 612, 612)


#multiplier <- seq(0.05, 1, by = 0.05) 
multiplier <- seq(0.01, 1, by = 0.01) 

########################################################################
########################################################################
# input list

species = list(s1, s2, s3)
tech = list(t1, t2)
cost = list(c1, c2)
baskets = list(b1)

######################################################################
# Make useful data frames of each of our lists: this allows any number to be added.
sp_df <- as.data.frame(do.call(rbind, species))
tech_df <- as.data.frame(do.call(rbind, tech))
cost_df <- as.data.frame(do.call(rbind, cost))
baskets_df <- as.data.frame(do.call(rbind, baskets))

# Calculate number of tech and species
ntech <- nrow(tech_df)
nspecies <- nrow(sp_df)

# Add starting stock values.
stock_df <- data.frame(t(sp_df$X))

# Make a few full data frames we can fill later
h_df <- data.frame(matrix(ncol = nspecies))
e_df <- data.frame(matrix(ncol = ntech))

## Profits:
pft_df <- data.frame(matrix(ncol = ntech))
rev_sp_df <- data.frame(matrix(ncol = nspecies))

#WWWW
# We still probably want to calculate harvest per species AND technology...
#WWWW


### --------
### Unchanging Parameters
### --------

# D is the cost matrix, where the diagonal values are the cost of each tech:
D <- matrix(0, nrow = ntech, ncol = ntech)
diag(D) <- 2*cost_df$cost

# Z is the the tech matrix
Z <- t(as.matrix(tech_df[,-1]))

# P is the price matrix
P <- as.matrix(sp_df$p)

# M is whether a species is contained in a basket:
M <- as.matrix(baskets_df)

# We build a diagonal matrix (nxn) for the second constraint
N <- matrix(0, nrow=ntech,ncol=ntech)
diag(N) <- 1

mortality=mortguess1

### --------
### Changing Parameters
### --------

for (i in 1:years){
  ### ------
  ### Calculate effort per year
  ### ------
  E <- NULL
  # B, where the diagonal corresponds to stock sizes (X) for each species
  B <- matrix(0, nrow = nspecies, ncol = nspecies)
  diag(B) <- as.numeric(tail(stock_df,1)) ### Stock goes here...
  # d combines the P,B,and Z matrices yielding the per tech revenue (1xtech)
  # (1xspecies)x(speciesxspecies)x(speciesxtech)
  d <- t(t(P)%*%B%*%Z)
  # A is the transpose matrix that defines which basket each species is in:
  A <- -1*M%*%B%*%Z
  # Add the second constraint N:
  A <- rbind(A, N)
  # Take the transpose for solve.QP:
  t_A <- t(A)
  # We need to calculate the basket caps based on the initial BASKET size(not stock size!):
  viz_stock <- rowSums(tail(stock_df,1)) ### And here...
  # Make the first constraint based on fixed quota
  b <- -120   
  # Make a second constraint so they're all greater than zero:
  b2 <- matrix(0, nrow=ntech, ncol=1)
  # Bind them together
  b <- rbind(b, b2)
  # Find optimal efforts
  
  E <- solve.QP(D,d,t_A,b, meq=0)$solution # Vector where the position is the effort per tech.
  
  E[E<0] <- 0
  
  ##E <- solve.QP(na.omit(D),na.omit(d), na.omit(t_A), na.omit(b), meq=0)$solution
  
  
  ### ------
  ### Calculate harvest for each species for the year
  ### ------
  # Repeat E for each species, so we have effort per tech per species:
  diag_b <- diag(B)
  
  h <- c(B%*%Z%*%E) # Position corresponds to the species:
  
  
  # diagonal
  
  
  h1 <- c(pmin(h, diag_b))
  
  # Calculate stock growth with X(t+1) = Xt + Xt*r*(1-Xt/K) - h
  stock_next <- tail(stock_df,1) + (sp_df$r)*tail(stock_df,1)*(1-tail(stock_df,1)/sp_df$K) - h
  
  stock_next <- pmax(stock_next,0)
  ### Calculate profit for each technology for the year
  ### -----
  
  # Rev per tech = revenue per unit effort of that tech x effort applied
  # (1xtech)*(techx1)
  rev_t <- t(d)*E
  
  # Cost per technology = cost per unit of effort for that tech * effort applied to that tech
  # Costs are available in the cost_df
  cpue <- cost_df$cost
  cost_t <- cpue*E
  
  # Profit per tech is the revenue - cost
  pft <- as.data.frame(rev_t-cost_t)
  names(pft) <- names(pft_df)
  
  ### Profit per species
  ### -----
  
  # Rev per species:
  
  rev_sp <- as.data.frame(h*t(P))
  names(rev_sp) <- names(rev_sp_df)
  
  # Add them to the corresponding data frames:
  stock_df <- rbind(stock_df, stock_next)
  h_df <- rbind(h_df, h)
  e_df <- rbind(e_df, E)
  pft_df <- rbind(pft_df, pft)
  rev_sp_df <- rbind(rev_sp_df, rev_sp)
  
}

# Make the output column names nice
sp_names <- paste("s", sp_df$s, sep = "_")
tech_names <- paste("t", tech_df$t, sep = "_")
# Make yr vector:
year <- seq(0, years, by = 1)
# Add stock/tech names & years
colnames(stock_df) <- sp_names
colnames(h_df) <- sp_names
colnames(rev_sp_df) <- sp_names

colnames(e_df) <- tech_names
colnames(pft_df) <- tech_names