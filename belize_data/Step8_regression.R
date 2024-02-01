# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(here)
library(broom)

#read
species_param_wide <- read_csv(here("belize_data", "step7", "species_param_wide.csv")) %>% 
  mutate(logr=log(r),
         logk=log(k))

species_history <- read_csv(here("belize_data", "step2", "belize_fish_life_history.csv")) %>% 
  select(-k)

#plot
# plot r and k changing y axis limits
graph_species1 <- ggplot(data=species_param_wide, aes(x=r, 
                                                 y=k)) + 
  geom_point(size=2) +
  geom_smooth(size=1.5) +
  labs(x="intrinsic growth", 
       y="carrying capacity",
       title= "Scatter graph r and k") + 
  scale_color_manual(values=c("D"="#008FD5", "I"="#77AB43","R"="#FF2700"))+ 
  ylim(0,9000)+
  theme_classic() 

graph_species1

# there are some outliers that might need to be eliminated

# plot r and k 
graph_species2 <- ggplot(data=species_param_wide, aes(x=r, 
                                                      y=k)) + 
  geom_point(size=2) +
  geom_smooth(size=1.5) +
  labs(x="intrinsic growth", 
       y="carrying capacity",
       title= "Scatter graph r and k") + 
  scale_color_manual(values=c("D"="#008FD5", "I"="#77AB43","R"="#FF2700"))+ 
  theme_minimal() 

graph_species2

#plot logr and k changing y axis limits
graph_species3 <- ggplot(data=species_param_wide, aes(x=logr, 
                                                      y=k)) + 
  geom_point(size=2) +
  geom_smooth(size=1.5) +
  labs(x="log(intrinsic growth)", 
       y="carrying capacity",
       title= "Scatter graph r and k") + 
  scale_color_manual(values=c("D"="#008FD5", "I"="#77AB43","R"="#FF2700"))+ 
  theme_minimal() 

graph_species3

#plot logk and r changing y axis limits
graph_species4 <- ggplot(data=species_param_wide, aes(x=r, 
                                                      y=logk)) + 
  geom_point(size=2) +
  geom_smooth(size=1.5) +
  labs(x="intrinsic growth", 
       y="log(carrying capacity)",
       title= "Scatter graph r and k") + 
  scale_color_manual(values=c("D"="#008FD5", "I"="#77AB43","R"="#FF2700"))+ 
  theme_minimal() 

graph_species4

#regression
lm1 <- lm(k ~ r, data=species_param_wide )
ols <- broom::tidy(lm1)

#regression
lm2 <- lm(k ~ logr, data=species_param_wide )
ols2 <- broom::tidy(lm2)

#regression
lm3 <- lm(logk ~ r, data=species_param_wide )
ols3 <- broom::tidy(lm3)

#rsquare
result_ols <- summary(lm3)$r.squared
#result_ols <- summary(lm1)$r.squared

# calculate k
# keep values of k
species_k <- species_param_wide %>% 
  select(species, k)

# join
species_merge <- merge(x=species_history,y=species_k, 
                          by.x="sci_name", 
                          by.y="species", all.x=TRUE) 

# estimate k 
#ols_constant <- c(ols[1,2])
#ols_constant_r <- ols_constant$estimate
#ols_param <- c(ols[2,2])
#ols_param_r <- ols_param$estimate

ols_constant <- c(ols3[1,2])
ols_constant_r <- ols_constant$estimate
ols_param <- c(ols3[2,2])
ols_param_r <- ols_param$estimate

species_est_k <- species_merge %>% 
  mutate(k_est=exp(ols_constant_r+r*ols_param_r))

#mojarra got a negative number!

write.csv(species_est_k, here("belize_data", "step8", "species_est_k.csv"), row.names = F)