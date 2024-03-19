# # other try

result2 <- results %>% 
  mutate(exploitation.rate2=harvest/biomass)%>% 
  select(-harvest, -exploitation.rate)

results2 <- result2 %>% 
  pivot_longer(cols=c(7:9),
               names_to='category',
               values_to='result') 

for (i in 1:13) {
  if (i != 6) {
    # Substitute the value of i into the code
    plot <- ggplot(data = results2 %>% filter(basket == i), aes(x = year, y = result, group = per_quota)) + 
      geom_line(aes(color = per_quota)) +
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
      labs(colour = "Quota percentage")
    
    # Store the plot in the list
    plot_list[[paste("results_b", i, sep = "")]] <- plot
    
    # Save the plot to a file in the "figures" folder
    ggsave(paste0("3 Belize/all_results/figures/plot_b", i, ".png"), plot = plot, width = 6, height = 4, dpi = 300)
  }
}

#results3 <- results %>% 
#  filter(effort.t_1<0)

#problem with basket 11 and 12