
p1 <- as.data.frame(extraction_df) %>%
  filter(region == 'East') %>%
  hist_fbuy_plot(., regions = 'East', panel = '1', upper = 100)
  
p2 <- as.data.frame(extraction_df) %>%
  filter(region == 'Central') %>%
  hist_fbuy_plot(., regions = 'Central', 
                 panel = '1', upper = 100)

p3 <- as.data.frame(extraction_df) %>%
  filter(region == 'West') %>%
  hist_fbuy_plot(., regions = 'West', 
                 panel = '1', upper = 100)

g <- arrangeGrob(p1, p2, p3, nrow = 1)
ggsave("results/region/fbuy/fbuy_histo.pdf", g, width = 10, height = 5, dpi=600, scale = 3, units = "cm") #saves g




