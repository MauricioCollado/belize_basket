# faceted graph

# package
rm(list=ls())
library(tidyverse)
library(here)
library(RColorBrewer)


#files
results <- read_csv(here("3 Belize", "all_results", "results", "final", "result_basket.csv")) 
#results[with(results, order(per_quota, year)),]

max_rate <- max(results$exploitation.rate, na.rm=T)


# load species data
species <- read_csv(here("belize_data", "dataset", "species_belize.csv")) 

# load k information
species_k <- species %>% 
  select(comm_name, basket, k_used) %>% 
  mutate(category="biomass",
         species=comm_name,
         k_limit=0.5*k_used) %>% 
  drop_na() #dropping na species

# r, price, q/r
species_other <- species %>%
  select(comm_name, basket, r_used, price, 24) 

#rename
colnames(species_other) <- c("species", "basket", "biomass", "revenue", "exploitation.rate")

# more treatment
species_other <- species_other %>%
  mutate(biomass=paste("r: ", round(biomass, 2), sep = " "),
         revenue=paste("$/ton: ", round(revenue, 0), sep = " "),
         exploitation.rate=paste("q/r: ", round(exploitation.rate, 3), sep = " "))

species_other1 <- species_other %>% 
  pivot_longer(cols=c(3:5),
               names_to='category',
               values_to='lab') 
  

##############################################################
# experiment
##############################################################

# TEST!!!
bio_b1 <- ggplot(data=results%>% filter(basket ==1), aes(x=year, y=biomass, group=per_quota)) + 
  geom_line(aes(color=per_quota)) +
  labs(x="Year", 
       y="Stock",
       title="") + 
  facet_wrap(vars(species), ncol=4)+
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        strip.text = element_text(size = 7))+
  labs(colour="Quota percentage")

bio_b1 

revenue_b1 <- ggplot(data=results%>% filter(basket ==1), aes(x=year, y=revenue, group=per_quota)) + 
  geom_line(aes(color=per_quota)) +
  labs(x="Year", 
       y="Stock",
       title="") + 
  facet_wrap(vars(species), ncol=4)+
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        strip.text = element_text(size = 7))+
  labs(colour="Quota percentage")

revenue_b1 

explo_b1 <- ggplot(data=results%>% filter(basket ==9), aes(x=year, y=exploitation.rate, group=per_quota)) + 
  geom_point(aes(color=per_quota)) +
  labs(x="Year", 
       y="Stock",
       title="") + 
  facet_wrap(vars(species), ncol=4)+
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        strip.text = element_text(size = 7))+
  labs(colour="Quota percentage")

explo_b1 


##############################################################
#need more datasets
##############################################################

result1 <- results %>% 
  select(-harvest)

results1 <- result1 %>% 
  pivot_longer(cols=c(7:9),
               names_to='category',
               values_to='result') 

#results1_k <- results1 %>% 
#  select(basket, per_quota, year, species, category) %>% 
#  filter(category=="biomass")

results1 <- full_join(results1, species_k, by=c("basket", "species", "category")) %>% 
  select(-comm_name)

results1 <- full_join(results1, species_other1, by=c("basket", "species", "category")) 

##############################################################
# trying FACET GRID
##############################################################
results_b1 <- ggplot(data=results1%>% filter(basket ==1), aes(x=year, y=result, group=per_quota)) + 
  geom_line(aes(color=per_quota)) +
  geom_line(aes(x=year, y=k_limit))+
  geom_text(aes(x=15, y=Inf-1, label=lab, vjust=1), size=1.5)+
  labs(x="Year", 
       y="",
       title="Basket 1") + 
  facet_grid(category ~ species, scales = "free_y", switch = "y")+
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        strip.text = element_text(size = 7))+
  labs(colour="Quota percentage")+
  scale_color_distiller(palette="Spectral")

results_b1 

results_b1 <- ggplot(data=results1%>% filter(basket ==1), aes(x=year, y=result, group=per_quota)) + 
  geom_line(aes(color=per_quota)) +
  labs(x="Year", 
       y="",
       title="Basket 1") + 
  facet_grid(category ~ species, scales = "free_y", switch = "y")+
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        strip.text = element_text(size = 7))+
  labs(colour="Quota percentage")


results_b1 


# let's do a loop TO GET ALL RESULTS

# Create an empty list to store the results
plot_list <- list()

# Loop from 1 to 13 (excluding 6 and 12)
for (i in 1:13) {
  if (i != 6) {
    # Substitute the value of i into the code
    plot <- ggplot(data = results1 %>% filter(basket == i), aes(x = year, y = result, group = per_quota)) + 
      geom_line(aes(color = per_quota)) +
      geom_line(aes(x=year, y=k_limit)) +
      geom_text(aes(x=15, y=Inf-1, label=lab, vjust=1), size=1.5) +
      labs(x = "Year", 
           y = "",
           title = paste("Basket", i)) + 
      facet_grid(category ~ species, scales = "free_y", switch = "y") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            legend.text = element_text(size = 6),
            axis.title.x = element_text(size = 6),
            strip.text = element_text(size = 7)) +
      labs(colour = "Quota percentage") +
      scale_color_distiller(palette="Spectral")
    
    # Store the plot in the list
    plot_list[[paste("results_b", i, sep = "")]] <- plot
    
    # Save the plot to a file in the "figures" folder
    ggsave(paste0("3 Belize/all_results/figures/plot_b", i, ".png"), plot = plot, width = 6, height = 4, dpi = 300)
  }
}


